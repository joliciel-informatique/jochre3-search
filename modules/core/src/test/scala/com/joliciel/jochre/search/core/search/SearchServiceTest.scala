package com.joliciel.jochre.search.core.search

import com.joliciel.jochre.ocr.core.model.Alto
import com.joliciel.jochre.search.core.lucene.JochreIndex
import com.joliciel.jochre.search.core.{DocMetadata, DocReference}
import zio.{Scope, ZIO, ZLayer}
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}

import scala.util.Using

object SearchServiceTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex {
  val docRef1 = DocReference("doc1")
  val metadata1 = DocMetadata(title = "Hello World")
  val altoXml =
    <alto>
      <Layout>
        <Page PHYSICAL_IMG_NR="1">
          <PrintSpace>
            <TextBlock>
              <TextLine HPOS="10" VPOS="10" WIDTH="110" HEIGHT="20">
                <String CONTENT="Hello" HPOS="10" VPOS="10" WIDTH="50" HEIGHT="20">
                  <ALTERNATIVE PURPOSE="Synonym">Hi</ALTERNATIVE>
                  <ALTERNATIVE PURPOSE="Synonym">Howdy</ALTERNATIVE>
                </String>
                <SP></SP>
                <String CONTENT="World" HPOS="70" VPOS="10" WIDTH="50" HEIGHT="20"></String>
              </TextLine>
            </TextBlock>
          </PrintSpace>
        </Page>
        <Page PHYSICAL_IMG_NR="2">
          <PrintSpace>
            <TextBlock>
              <TextLine HPOS="10" VPOS="10" WIDTH="120" HEIGHT="20">
                <String CONTENT="Nice" HPOS="10" VPOS="10" WIDTH="40" HEIGHT="20">
                  <ALTERNATIVE PURPOSE="Synonym">Pleasant</ALTERNATIVE>
                  <ALTERNATIVE PURPOSE="Synonym">Lovely</ALTERNATIVE>
                </String>
                <SP></SP>
                <String CONTENT="day" HPOS="60" VPOS="10" WIDTH="30" HEIGHT="20"></String>
                <SP></SP>
                <String CONTENT="to" HPOS="100" VPOS="10" WIDTH="20" HEIGHT="20"></String>
                <HYP CONTENT="-" HPOS="120" VPOS="10" WIDTH="10" HEIGHT="20"></HYP>
              </TextLine>
              <TextLine HPOS="10" VPOS="40" WIDTH="30" HEIGHT="20">
                <String CONTENT="day" HPOS="10" VPOS="40" WIDTH="30" HEIGHT="20"></String>
              </TextLine>
            </TextBlock>
          </PrintSpace>
        </Page>
      </Layout>
    </alto>

  val alto1 = Alto.fromXML(altoXml)

  val docRef2 = DocReference("doc2")
  val metadata2 = DocMetadata(title = "Hello people")

  val altoXml2 =
    <alto>
      <Layout>
        <Page PHYSICAL_IMG_NR="1">
          <PrintSpace>
            <TextBlock>
              <TextLine HPOS="10" VPOS="10" WIDTH="120" HEIGHT="20">
                <String CONTENT="Hello" HPOS="10" VPOS="10" WIDTH="50" HEIGHT="20"></String>
                <SP></SP>
                <String CONTENT="people" HPOS="70" VPOS="10" WIDTH="50" HEIGHT="20"></String>
                <String CONTENT="." HPOS="120" VPOS="10" WIDTH="10" HEIGHT="20"></String>
              </TextLine>
              <TextLine HPOS="10" VPOS="30" WIDTH="130" HEIGHT="20">
                <String CONTENT="How" HPOS="10" VPOS="30" WIDTH="30" HEIGHT="20"></String>
                <SP></SP>
                <String CONTENT="are" HPOS="50" VPOS="30" WIDTH="30" HEIGHT="20"></String>
                <SP></SP>
                <String CONTENT="you" HPOS="90" VPOS="30" WIDTH="30" HEIGHT="20"></String>
                <String CONTENT="?" HPOS="120" VPOS="30" WIDTH="10" HEIGHT="20"></String>
              </TextLine>
              <TextLine HPOS="10" VPOS="150" WIDTH="130" HEIGHT="20">
                <String CONTENT="Fine" HPOS="10" VPOS="50" WIDTH="40" HEIGHT="20"></String>
                <SP></SP>
                <String CONTENT="thank" HPOS="60" VPOS="50" WIDTH="50" HEIGHT="20"></String>
                <SP></SP>
                <String CONTENT="you" HPOS="120" VPOS="50" WIDTH="30" HEIGHT="20"></String>
                <String CONTENT="." HPOS="150" VPOS="50" WIDTH="10" HEIGHT="20"></String>
              </TextLine>
            </TextBlock>
          </PrintSpace>
        </Page>
      </Layout>
    </alto>

  val alto2 = Alto.fromXML(altoXml2)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("SearchServiceTest")(
    test("index alto file") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        pageCount1 <- searchService.indexAlto(docRef1, alto1, metadata1)
        pageCount2 <- searchService.indexAlto(docRef2, alto2, metadata2)
        index <- ZIO.service[JochreIndex]
        refsWorld <- ZIO.attempt {
          val query = SearchQuery(Contains("World"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHello <- ZIO.attempt {
          val query = SearchQuery(Contains("Hello"))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
        refsHiWorld <- ZIO.attempt {
          val query = SearchQuery(Contains("\"Hi World\""))
          Using(index.searcherManager.acquire()) { searcher =>
            searcher.findMatchingRefs(query)
          }.get
        }
      } yield {
        assertTrue(pageCount1 == 2) &&
        assertTrue(pageCount2 == 1) &&
        assertTrue(refsWorld == Seq(docRef1)) &&
        assertTrue(refsHiWorld == Seq(docRef1)) &&
        assertTrue(refsHello.toSet == Set(docRef1, docRef2))
      }
    },
    test("search alto file") {
      for {
        _ <- getSearchRepo()
        searchService <- ZIO.service[SearchService]
        _ <- searchService.indexAlto(docRef1, alto1, metadata1)
        _ <- searchService.indexAlto(docRef2, alto2, metadata2)
        resultAre <- searchService.search("are", 0, 100, None, None, "test")
      } yield {
        println(f"$resultAre")
        assertTrue(resultAre.results.head.snippets.head.text == "How <b>are</b> you?")
      }
    }
  ).provideLayer(
    (searchRepoLayer ++ indexLayer) >>> SearchService.live ++ ZLayer.service[SearchRepo] ++ ZLayer.service[JochreIndex]
  ) @@ TestAspect.sequential
}
