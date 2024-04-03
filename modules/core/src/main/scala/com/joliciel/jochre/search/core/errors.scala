package com.joliciel.jochre.search.core

class NotFoundException(message: String) extends Exception(message)
class BadRequestException(message: String) extends Exception(message)

class UnknownFieldException(message: String) extends BadRequestException(message)
class WrongFieldTypeException(message: String) extends BadRequestException(message)
class NoSearchCriteriaException(message: String) extends BadRequestException(message)
