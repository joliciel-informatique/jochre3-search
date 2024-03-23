package com.joliciel.jochre.search.core.util

object AndThenIf {
  object Implicits {
    implicit class extendedFunction1[R](f: Function1[R, R]) {
      def andThenIf(condition: Boolean)(g: R => R): Function1[R, R] = AndThenIf(f, condition, g)
    }
  }

  def apply[R](f: Function1[R, R], condition: Boolean, g: R => R): Function1[R, R] =
    if (condition) {
      f.andThen(g)
    } else {
      f
    }
}
