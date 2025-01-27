package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.WithTestIndex
import com.joliciel.jochre.search.core.lucene.{AnalyzerGroup, JochreIndex}
import org.apache.lucene.store.MMapDirectory
import zio.ZLayer

import java.io.File
import com.joliciel.jochre.search.core.WithYiddishTestIndex

trait WithYiddishTestIndexLayer extends WithYiddishTestIndex {
  val indexLayer: ZLayer[Any, Nothing, JochreIndex] =
    ZLayer.succeed {
      getJochreIndex
    }
}
