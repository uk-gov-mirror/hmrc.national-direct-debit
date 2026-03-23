/*
 * Copyright 2025 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.nationaldirectdebit.services

import play.api.Logging
import uk.gov.hmrc.auth.core.{Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.nationaldirectdebit.connectors.ChrisConnector
import uk.gov.hmrc.nationaldirectdebit.models.{FATAL_ERROR as SubmissionStatusFatalError, SUBMITTED as SubmissionStatusSubmitted, SubmissionResult}
import uk.gov.hmrc.nationaldirectdebit.models.requests.{AuthenticatedRequest, ChrisSubmissionRequest}
import uk.gov.hmrc.nationaldirectdebit.models.requests.chris.{DirectDebitSource, EnvelopeDetails}
import uk.gov.hmrc.nationaldirectdebit.services.ChrisEnvelopeConstants.{enrolmentToHodService, hodServiceToKnownFactType}
import uk.gov.hmrc.nationaldirectdebit.services.chrisUtils.SchemaValidator
import uk.gov.hmrc.play.audit.http.connector.AuditResult

import scala.util.{Failure, Success}
import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ChrisService @Inject() (chrisConnector: ChrisConnector, validator: SchemaValidator, auditService: AuditService)(implicit ec: ExecutionContext)
    extends Logging {

  private val enrolmentHasKnownFacts =
    (enrolmentKey: String) => hodServiceToKnownFactType.keys.exists(_ == enrolmentToHodService(enrolmentKey))

  private val enrolmentsRelevantToChRIS =
    (e: Enrolment) => (e.isActivated || e.state == "NotYetActivated") && enrolmentToHodService.keys.toSet(e.key)

  private def getActiveEnrolmentForKeys(
    serviceType: DirectDebitSource
  )(implicit request: AuthenticatedRequest[?]): Seq[Map[String, String]] = {

    val expectedHodServiceOpt = ChrisEnvelopeConstants.directDebitSourceToHodService.get(serviceType)

    // Only activated enrolments
    val active = request.enrolments.filter(enrolmentsRelevantToChRIS)

    // Priority ordering for UTR-based enrolments
    val utrPriority: Map[String, Int] = Map(
      "IR-SA-PART-ORG"  -> 1,
      "IR-SA-TRUST-ORG" -> 2,
      "IR-SA"           -> 3
    ).withDefaultValue(99)

    // Map each enrolment → HoD service → knownFact
    val mapped: Seq[(String, String, Int)] =
      active.toSeq.flatMap {
        case Enrolment(enrolmentKey, identifiers, _, _) if enrolmentHasKnownFacts(enrolmentKey) =>
          val hodService = ChrisEnvelopeConstants.enrolmentToHodService(enrolmentKey)
          val knownFactType = ChrisEnvelopeConstants.hodServiceToKnownFactType(hodService)

          val value = hodService match {
            case "PAYE" | "CIS" =>
              identifiers.find(_.key == "TaxOfficeNumber").map(_.value).getOrElse("") +
                identifiers.find(_.key == "TaxOfficeReference").map(_.value).getOrElse("")
            case _ =>
              identifiers.headOption.fold("") { case EnrolmentIdentifier(key, v) =>
                if (key == "NINO") v.trim.take(8) else v.trim
              }
          }
          Some((knownFactType, value, utrPriority(enrolmentKey)))
        case _ => None
      }

    // Deduplicate by knownFactType and apply EMPREF rules
    val deduped: Seq[Map[String, String]] =
      mapped
        .groupBy(_._1) // group by knownFactType
        .toSeq
        .flatMap { case (knownFactType, entries) =>
          val sorted = entries.sortBy(_._3) // apply UTR priority

          if (knownFactType == "EMPREF") {
            // Keep ALL EMPREF entries
            sorted.map { case (_, value, _) =>
              Map("knownFactType" -> knownFactType, "knownFactValue" -> value)
            }
          } else {
            // Keep only highest priority one
            val (_, bestValue, _) = sorted.head
            Seq(Map("knownFactType" -> knownFactType, "knownFactValue" -> bestValue))
          }
        }

    // Reorder to put expected service's knownFactType first
    expectedHodServiceOpt match {
      case Some(expectedService) =>
        val expectedKnownFactType = hodServiceToKnownFactType.getOrElse(expectedService, "")
        val (matching, others) = deduped.partition(_("knownFactType") == expectedKnownFactType)
        matching ++ others

      case None =>
        deduped
    }
  }

  private def knownFactDataWithEnrolment(
    serviceType: DirectDebitSource
  )(implicit request: AuthenticatedRequest[?]): Seq[Map[String, String]] = {

    val expectedHodServiceOpt = ChrisEnvelopeConstants.directDebitSourceToHodService.get(serviceType)

    logger.debug(s"Retrieved enrolments: ${request.enrolments.map(_.key).mkString(", ")}")
    logger.info(s"Expected HOD service for [$serviceType] = ${expectedHodServiceOpt.getOrElse("not found")}")

    val saKeys = Set("IR-SA-PART-ORG", "IR-SA", "IR-SA-TRUST-ORG")

    val (saEnrolments, otherEnrolments) =
      request.enrolments.toSeq
        .filter(enrolmentsRelevantToChRIS)
        .partition(e => saKeys(e.key))

    // --------------------------------------------------------
    // NEW: Collapse all SA enrolments into ONE
    // --------------------------------------------------------
    val activeEnrolments = saEnrolments.take(1) ++ otherEnrolments

    logger.debug(s"SA keys originally present: $saEnrolments")
    logger.debug(s"SA key retained for known fact: ${saEnrolments.take(1)}")

    // Build output ONLY for properly mapped enrolments + HOD services
    val mappedFacts: Seq[Map[String, String]] =
      activeEnrolments.flatMap {
        case Enrolment(enrolmentKey, identifiers, _, _) if enrolmentHasKnownFacts(enrolmentKey) =>
          val hodService = enrolmentToHodService(enrolmentKey)
          val knownFactType = hodServiceToKnownFactType(hodService)

          val value = hodService match {
            case "PAYE" | "CIS" =>
              identifiers.find(_.key == "TaxOfficeNumber").map(_.value).getOrElse("") +
                identifiers.find(_.key == "TaxOfficeReference").map(_.value).getOrElse("")
            case _ =>
              identifiers.headOption.fold("") { case EnrolmentIdentifier(key, v) =>
                if (key == "NINO") v.trim.take(8) else v.trim
              }
          }

          Some(
            Map(
              "service"         -> hodService,
              "identifierName"  -> knownFactType,
              "identifierValue" -> value
            )
          )
        case _ => None
      }

    // Reorder: matching first, then all others
    expectedHodServiceOpt match {
      case Some(expected) =>
        val (matching, others) = mappedFacts.partition(_("service") == expected)
        matching ++ others

      case None =>
        mappedFacts
    }
  }

  def submitToChris(
    submission: ChrisSubmissionRequest
  )(implicit hc: HeaderCarrier, request: AuthenticatedRequest[?]): Future[SubmissionResult] = {
    val correlationId: String = UUID.randomUUID().toString.replace("-", "")

    val knownFactData = knownFactDataWithEnrolment(submission.serviceType)
    val keysData = getActiveEnrolmentForKeys(submission.serviceType)
    val envelopeDetails = EnvelopeDetails.details(submission, request, knownFactData, keysData, correlationId)

    audit(envelopeDetails, correlationId)

    validator.validate(envelopeDetails.build) match {
      case Failure(exception) => throw exception
      case Success(_) =>
        chrisConnector.submitEnvelope(envelopeDetails.build, correlationId) map {
          case submissionResult if submissionResult.status == SubmissionStatusSubmitted =>
            logger.debug(s"ChRIS submission successful for a correlationId = $correlationId")
            submissionResult
          case _ =>
            val message =
              s"ChRIS submission failed with " +
                s"SubmissionStatus = ${SubmissionStatusFatalError.name}, " +
                s"${request.sessionData}, " +
                s"correlationId = $correlationId"
            throw new RuntimeException(message)
        }
    }
  }

  private def audit(envelopeDetails: EnvelopeDetails, correlationId: String)(implicit
    hc: HeaderCarrier,
    request: AuthenticatedRequest[?]
  ): Future[String] = {
    auditService.sendEvent(envelopeDetails) map { result =>
      val message = s"{$result} for correlationId = $correlationId, ${request.sessionData}"
      result match {
        case AuditResult.Success => message
        case _ =>
          logger.error(message)
          throw new RuntimeException(message)
      }
    }
  }

}
