package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.search.DocRev

case class AltoDocument(
    ref: DocReference,
    rev: DocRev,
    text: String,
    metadata: DocMetadata
)
