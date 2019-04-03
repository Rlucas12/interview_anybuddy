package com.rlucas.db

import cats.effect.IO
import doobie.util.transactor.Transactor

import scala.concurrent.ExecutionContext

trait Database {

  implicit val cs = IO.contextShift(ExecutionContext.global)
  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5433/anybuddy",
    "root",
    "root"
  )

}
