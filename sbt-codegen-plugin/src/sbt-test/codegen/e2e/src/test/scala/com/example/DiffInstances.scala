package com.example

import com.softwaremill.diffx.Diff

trait DiffInstances {
  implicit val tagDiff = Diff.derived[Tag].ignore[Tag, Option[Long]](_.id)
  implicit val statusDiff = Diff.derived[Status]
  implicit val categoryDiff =
    Diff.derived[Category].ignore[Category, Option[Long]](_.id)
  implicit val petDiff = Diff.derived[Pet].ignore[Pet, Option[Long]](_.id)
}
