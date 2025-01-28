package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.IndexField
import org.apache.lucene.facet.FacetsConfig

private[lucene] object FacetConfigHolder {
  val facetsConfig: FacetsConfig = new FacetsConfig()

  IndexField.values.filter(_.aggregatable).foreach { field =>
    val dimConfig = facetsConfig.getDimConfig(field.fieldName)
    dimConfig.multiValued = field.isMultiValue
    dimConfig.requireDimCount = true
  }
}
