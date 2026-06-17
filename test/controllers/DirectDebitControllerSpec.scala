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

package controllers

import actions.FakeAuthAction
import base.SpecBase
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.http.Status.{BAD_REQUEST, CONFLICT, OK}
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.Helpers.{contentAsJson, status}
import uk.gov.hmrc.http.{BadGatewayException, HeaderCarrier}
import uk.gov.hmrc.nationaldirectdebit.controllers.DirectDebitController
import uk.gov.hmrc.nationaldirectdebit.models.requests.chris.*
import uk.gov.hmrc.nationaldirectdebit.models.requests.{AuthenticatedRequest, ChrisSubmissionRequest, GenerateDdiRefRequest, PaymentPlanDuplicateCheckRequest, WorkingDaysOffsetRequest}
import uk.gov.hmrc.nationaldirectdebit.models.responses.*
import uk.gov.hmrc.nationaldirectdebit.models.{SUBMITTED, SubmissionResult}
import uk.gov.hmrc.nationaldirectdebit.services.{ChrisService, DirectDebitService}

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.Future

class DirectDebitControllerSpec extends SpecBase {

  "DirectDebitController" - {
    "retrieveDirectDebits method" - {
      "return 200 and a successful response when the max number of records is supplied" in new SetUp {
        when(mockDirectDebitService.retrieveDirectDebits()(any()))
          .thenReturn(Future.successful(testDataCacheResponse))

        val result: Future[Result] = controller.retrieveDirectDebits()(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testDataCacheResponse)
      }

      "return 200 and a successful response with 0 when the no value of max records is supplied" in new SetUp {
        when(mockDirectDebitService.retrieveDirectDebits()(any()))
          .thenReturn(Future.successful(testEmptyDataCacheResponse))

        val result: Future[Result] = controller.retrieveDirectDebits()(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testEmptyDataCacheResponse)
      }
    }

    "AddFutureWorkingDaysOffset method" - {
      "return 200 and a successful response when the request is valid" in new SetUp {
        when(mockDirectDebitService.getWorkingDaysOffset(any())(any()))
          .thenReturn(Future.successful(testResponseModel))

        val result: Future[Result] = controller.getWorkingDaysOffset()(fakeRequestWithJsonBody(Json.toJson(testRequestModel)))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testResponseModel)
      }

      "return 400 when the request is not valid" in new SetUp {
        val result: Future[Result] = controller.getWorkingDaysOffset()(fakeRequestWithJsonBody(Json.toJson("invalid json")))

        status(result) mustBe BAD_REQUEST
      }
    }

    "generateDdiReference method" - {
      "return 200 and a successful response when the request is valid" in new SetUp {
        when(mockDirectDebitService.generateDdiReference(any())(any()))
          .thenReturn(Future.successful(GenerateDdiRefResponse("123")))

        val result: Future[Result] = controller.generateDdiReference()(fakeRequestWithJsonBody(Json.toJson(testDdiRefRequestModel)))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(GenerateDdiRefResponse("123"))
      }

      "return 409 and a when the generateDdiReference returns 502" in new SetUp {

        when(mockDirectDebitService.generateDdiReference(any())(any()))
          .thenReturn(Future.failed(new BadGatewayException("Failed to generate DDI Reference.")))

        val result: Future[Result] =
          controller.generateDdiReference()(fakeRequestWithJsonBody(Json.toJson(testDdiRefRequestModel)))

        status(result) mustBe CONFLICT
      }

      "return 400 when the request is not valid" in new SetUp {
        val result: Future[Result] = controller.generateDdiReference()(fakeRequestWithJsonBody(Json.toJson(789)))

        status(result) mustBe BAD_REQUEST
      }
    }

    "retrieveDirectDebitPaymentPlans method" - {
      "return 200 and a successful response when payment plans exist" in new SetUp {
        when(mockDirectDebitService.retrieveDirectDebitPaymentPlans(any())(any()))
          .thenReturn(Future.successful(testDDPaymentPlansCacheResponse))

        val result: Future[Result] = controller.retrieveDirectDebitPaymentPlans("test directDebitReference")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testDDPaymentPlansCacheResponse)
      }

      "return 200 and a successful response with 0 when no payment plans" in new SetUp {
        when(mockDirectDebitService.retrieveDirectDebitPaymentPlans(any())(any()))
          .thenReturn(Future.successful(testDDPaymentPlansEmptyResponse))

        val result: Future[Result] = controller.retrieveDirectDebitPaymentPlans("test directDebitReference")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testDDPaymentPlansEmptyResponse)
      }
    }

    "submitToChris method for SA submissions" - {

      "return 200 and success response when submission succeeds" in new SetUp {
        implicit val hc: HeaderCarrier = HeaderCarrier()

        when(
          mockChrisService.submitToChris(
            any[ChrisSubmissionRequest]()
          )(any[HeaderCarrier](), any[AuthenticatedRequest[_]])
        ).thenReturn(
          Future.successful(
            SubmissionResult(
              status = SUBMITTED,
              rawXml = Some("<Confirmation>TC Message received</Confirmation>")
            )
          )
        )

        val result: Future[Result] =
          controller.submitToChris()(fakeRequestWithJsonBody(Json.toJson(testChrisRequestSAMonthly)))

        status(result) mustBe OK
        (contentAsJson(result) \ "success").as[Boolean] mustBe true
        (contentAsJson(result) \ "response" \ "rawXml").as[String] must include("TC Message received")
      }

      "return 500 and error response when submission fails" in new SetUp {
        implicit val hc: HeaderCarrier = HeaderCarrier()

        when(
          mockChrisService.submitToChris(
            any[ChrisSubmissionRequest]()
          )(any[HeaderCarrier](), any[AuthenticatedRequest[_]])
        ).thenReturn(Future.failed(new RuntimeException("Boom!")))

        val result: Future[Result] =
          controller.submitToChris()(fakeRequestWithJsonBody(Json.toJson(testChrisRequestSAMonthly)))

        status(result) mustBe 500
        (contentAsJson(result) \ "success").as[Boolean] mustBe false
        (contentAsJson(result) \ "message").as[String] must include("Boom")
      }

    }

    "retrievePaymentPlanDetails method" - {
      "return 200 and a successful response when payment plans exist" in new SetUp {
        when(mockDirectDebitService.retrievePaymentPlanDetails(any(), any())(any()))
          .thenReturn(Future.successful(testPaymentPlanResponse))

        val result: Future[Result] = controller.retrievePaymentPlanDetails("test-dd-reference", "test-pp-reference")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testPaymentPlanResponse)
      }
    }

    "lockPaymentPlan method" - {
      "return 200 and a successful response when payment plans exist" in new SetUp {
        when(mockDirectDebitService.lockPaymentPlan(any(), any())(any()))
          .thenReturn(Future.successful(RDSPaymentPlanLock(lockSuccessful = true)))

        val result: Future[Result] = controller.lockPaymentPlan("test-dd-reference", "test-pp-reference")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(RDSPaymentPlanLock(lockSuccessful = true))
      }
    }

    "isDuplicatePaymentPlan method" - {
      "return 200 and a successful response when duplicate payment plan exist" in new SetUp {
        implicit val hc: HeaderCarrier = HeaderCarrier()
        when(
          mockDirectDebitService.isDuplicatePaymentPlan(
            any[PaymentPlanDuplicateCheckRequest]()
          )(any[HeaderCarrier]())
        ).thenReturn(Future.successful(DuplicateCheckResponse(true)))

        val result: Future[Result] =
          controller.isDuplicatePaymentPlan("test isDuplicatePaymentPlan")(fakeRequestWithJsonBody(Json.toJson(duplicateCheckRequest)))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(DuplicateCheckResponse(true))
      }
    }

    "isAdvanceNoticePresent method" - {
      "return 200 and a successful response when advance notice details exist" in new SetUp {
        val currentTime: LocalDateTime = LocalDateTime.now().withNano(0)
        when(mockDirectDebitService.isAdvanceNoticePresent(any(), any())(any()))
          .thenReturn(
            Future.successful(
              AdvanceNoticeResponse(
                totalAmount = Some(500),
                dueDate     = Some(currentTime.toLocalDate.plusMonths(1))
              )
            )
          )

        val result: Future[Result] = controller.isAdvanceNoticePresent("test-dd-reference", "test-pp-reference")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(
          AdvanceNoticeResponse(
            totalAmount = Some(500),
            dueDate     = Some(currentTime.toLocalDate.plusMonths(1))
          )
        )
      }

      "return 200 and a successful response when advance notice details does not exist" in new SetUp {
        val currentTime: LocalDateTime = LocalDateTime.now().withNano(0)
        when(mockDirectDebitService.isAdvanceNoticePresent(any(), any())(any()))
          .thenReturn(
            Future.successful(
              AdvanceNoticeResponse(
                totalAmount = None,
                dueDate     = None
              )
            )
          )

        val result: Future[Result] = controller.isAdvanceNoticePresent("test-dd-reference", "test-pp-reference")(fakeRequest)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(
          AdvanceNoticeResponse(
            totalAmount = None,
            dueDate     = None
          )
        )
      }
    }
  }

  class SetUp {
    val mockDirectDebitService: DirectDebitService = mock[DirectDebitService]
    val mockChrisService: ChrisService = mock[ChrisService]

    val testResponseModel: EarliestPaymentDateResponse = EarliestPaymentDateResponse(LocalDate.of(2025, 12, 13))
    val testRequestModel: WorkingDaysOffsetRequest = WorkingDaysOffsetRequest(LocalDate.of(2025, 12, 5), 8)
    val testDdiRefRequestModel: GenerateDdiRefRequest = GenerateDdiRefRequest("12345")
    val testEmptyDataCacheResponse: RDSDatacacheResponse = RDSDatacacheResponse(0, Seq.empty)
    val testDataCacheResponse: RDSDatacacheResponse = RDSDatacacheResponse(
      2,
      Seq(
        RDSDirectDebitDetails("testRef", LocalDateTime.of(2025, 12, 12, 12, 12), "testCode", "testNumber", "testName", true, 1),
        RDSDirectDebitDetails("testRef", LocalDateTime.of(2025, 12, 12, 12, 12), "testCode", "testNumber", "testName", true, 1)
      )
    )

    val fakeAuthAction = new FakeAuthAction(
      bodyParsers  = bodyParsers,
      testCredId   = "cred-123",
      testAffinity = "Individual"
    )

    // --- Add these Chris request test data here ---
    val baseChrisRequest: ChrisSubmissionRequest = ChrisSubmissionRequest(
      serviceType                = DirectDebitSource.TC,
      paymentPlanType            = PaymentPlanType.TaxCreditRepaymentPlan,
      paymentPlanReferenceNumber = None,
      paymentFrequency           = Some(PaymentsFrequency.Monthly),
      yourBankDetailsWithAuddisStatus = YourBankDetailsWithAuddisStatus(
        accountHolderName = "Test",
        sortCode          = "123456",
        accountNumber     = "12345678",
        auddisStatus      = false,
        accountVerified   = false
      ),
      planStartDate             = Some(PlanStartDateDetails(LocalDate.of(2025, 9, 1), "2025-09-01")),
      planEndDate               = None,
      paymentDate               = Some(PaymentDateDetails(LocalDate.of(2025, 9, 15), "2025-09-01")),
      yearEndAndMonth           = None,
      ddiReferenceNo            = "DDI123456789",
      paymentReference          = "testReference",
      totalAmountDue            = Some(BigDecimal(200)),
      amendPaymentAmount        = Some(BigDecimal(100)),
      paymentAmount             = Some(BigDecimal(100.00)),
      regularPaymentAmount      = Some(BigDecimal(90.00)),
      calculation               = None,
      suspensionPeriodRangeDate = None
    )

    val testChrisRequestSAMonthly: ChrisSubmissionRequest = baseChrisRequest.copy(
      serviceType                = DirectDebitSource.SA,
      paymentPlanType            = PaymentPlanType.BudgetPaymentPlan,
      paymentPlanReferenceNumber = None,
      paymentFrequency           = Some(PaymentsFrequency.Monthly)
    )

    val testChrisRequestSAWeekly: ChrisSubmissionRequest = baseChrisRequest.copy(
      serviceType                = DirectDebitSource.SA,
      paymentPlanType            = PaymentPlanType.BudgetPaymentPlan,
      paymentPlanReferenceNumber = None,
      paymentFrequency           = Some(PaymentsFrequency.Weekly)
    )

    val testDDPaymentPlansEmptyResponse: RDSDDPaymentPlansResponse = RDSDDPaymentPlansResponse(
      bankSortCode      = "sort code",
      bankAccountNumber = "account number",
      bankAccountName   = "account name",
      auDdisFlag        = "dd",
      paymentPlanCount  = 0,
      paymentPlanList   = Seq.empty
    )

    val testDDPaymentPlansCacheResponse: RDSDDPaymentPlansResponse = RDSDDPaymentPlansResponse(
      bankSortCode      = "sort code",
      bankAccountNumber = "account number",
      bankAccountName   = "account name",
      auDdisFlag        = "dd",
      paymentPlanCount  = 2,
      paymentPlanList = Seq(
        RDSPaymentPlan(
          scheduledPaymentAmount = Some(100),
          planRefNumber          = "ref number 1",
          planType               = "01",
          paymentReference       = "payment ref 1",
          hodService             = "CESA",
          submissionDateTime     = LocalDateTime.of(2025, 12, 12, 12, 12)
        ),
        RDSPaymentPlan(
          scheduledPaymentAmount = Some(100),
          planRefNumber          = "ref number 1",
          planType               = "01",
          paymentReference       = "payment ref 1",
          hodService             = "CESA",
          submissionDateTime     = LocalDateTime.of(2025, 12, 12, 12, 12)
        )
      )
    )

    private val currentTime = LocalDateTime.MIN

    val testPaymentPlanResponse: RDSPaymentPlanResponse = RDSPaymentPlanResponse(
      directDebitDetails = DirectDebitDetail(bankSortCode = Some("sort code"),
                                             bankAccountNumber  = Some("account number"),
                                             bankAccountName    = None,
                                             auDdisFlag         = true,
                                             submissionDateTime = currentTime
                                            ),
      paymentPlanDetails = PaymentPlanDetail(
        hodService                = "CESA",
        planType                  = "01",
        paymentReference          = "payment reference",
        submissionDateTime        = currentTime,
        scheduledPaymentAmount    = Some(1000),
        scheduledPaymentStartDate = Some(currentTime.toLocalDate),
        initialPaymentStartDate   = Some(currentTime.toLocalDate),
        initialPaymentAmount      = Some(150),
        scheduledPaymentEndDate   = Some(currentTime.toLocalDate),
        scheduledPaymentFrequency = Some(1),
        suspensionStartDate       = Some(currentTime.toLocalDate),
        suspensionEndDate         = None,
        balancingPaymentAmount    = Some(600),
        balancingPaymentDate      = Some(currentTime.toLocalDate),
        totalLiability            = None,
        paymentPlanEditable       = false
      )
    )

    val controller = new DirectDebitController(fakeAuthAction, mockDirectDebitService, mockChrisService, cc)

    val duplicateCheckRequest: PaymentPlanDuplicateCheckRequest = PaymentPlanDuplicateCheckRequest(
      directDebitReference = "testRef",
      paymentPlanReference = "payment ref 123",
      planType             = "01",
      paymentService       = "CESA",
      paymentReference     = "payment ref",
      paymentAmount        = Some(120.00),
      totalLiability       = Some(120.00),
      paymentFrequency     = Some(1),
      paymentStartDate     = Some(currentTime.toLocalDate)
    )
  }
}
