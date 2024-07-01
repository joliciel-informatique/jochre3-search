package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.authentication.RawAuthenticationProviderForTestsOnly
import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.service.{DatabaseTestBase, PreferenceService, SearchService, WithTestIndex}
import com.joliciel.jochre.search.yiddish.YiddishFilters
import doobie.Transactor
import sttp.client3._
import sttp.client3.impl.zio.RIOMonadAsyncError
import sttp.client3.testing.SttpBackendStub
import sttp.model.HeaderNames
import sttp.tapir.server.stub.TapirStubInterpreter
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, Task, ZLayer}

import scala.concurrent.ExecutionContext

object IndexAppTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex {

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("IndexAppTest")(
    test("call putPdfHttp endpoint") {
      val authenticationProvider = RawAuthenticationProviderForTestsOnly
      val executionContext = ExecutionContext.global
      val indexApp = IndexApp(authenticationProvider, executionContext)

      val myEndpoint: _root_.sttp.tapir.ztapir.ZServerEndpoint[Requirements, Any] = indexApp.putPdfHttp

      val stub = TapirStubInterpreter(SttpBackendStub(new RIOMonadAsyncError[Requirements]))
        .whenServerEndpointRunLogic(myEndpoint)
        .backend()

      val pdfStream = getClass.getResourceAsStream("/nybc200089-11-12.pdf")
      val altoStream = getClass.getResourceAsStream("/nybc200089-11-12_alto4.zip")
      val metadataStream = getClass.getResourceAsStream("/nybc200089_meta.xml")

      for {
        response <- basicRequest
          .contentType("application/json")
          .multipartBody(
            multipart("docReference", "nybc200089"),
            multipart("pdfFile", pdfStream),
            multipart("altoFile", altoStream),
            multipart("metadataFile", metadataStream)
          )
          .header(
            HeaderNames.Authorization,
            raw"""Bearer {"username": "Test", "email": "test@example.com", "roles": ["index"]}"""
          )
          .put(uri"http://test.com/index/pdf")
          .send(stub)
      } yield {
        assertTrue(response.body.getOrElse("") == """{"pages": 2}""")
      }
    }
  ).provideLayer(
    (transactorLayer ++ searchRepoLayer ++ suggestionRepoLayer ++ preferenceRepoLayer ++ indexLayer ++ YiddishFilters.live) >>> AppConfig.live ++ SearchService.live ++ PreferenceService.live ++ ZLayer
      .service[Transactor[Task]]
  ) @@ TestAspect.sequential
}
