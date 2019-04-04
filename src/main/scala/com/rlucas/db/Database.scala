package com.rlucas.db

import doobie._
import doobie.implicits._
import cats.effect.IO
import com.rlucas.models.FlatVevent
import com.rlucas.models.ICalendar._

import scala.concurrent.ExecutionContext

trait Database {

  implicit val cs = IO.contextShift(ExecutionContext.global)
  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5433/anybuddy",
    "root",
    "root"
  )

  def insertVevent(flatVevent: FlatVevent) = {
    sql"""
         |INSERT INTO vevent VALUES
         |(
         |  ${flatVevent.dtstamp},
         |  ${flatVevent.uid},
         |  ${flatVevent.dtstart},
         |  ${flatVevent.`class`},
         |  ${flatVevent.description},
         |  ${flatVevent.geo},
         |  ${flatVevent.lastMod},
         |  ${flatVevent.location},
         |  ${flatVevent.organizer},
         |  ${flatVevent.priority},
         |  ${flatVevent.seq},
         |  ${flatVevent.status},
         |  ${flatVevent.summary},
         |  ${flatVevent.transp},
         |  ${flatVevent.url},
         |  ${flatVevent.recurid},
         |  ${flatVevent.rrule},
         |  ${flatVevent.dtend},
         |  ${flatVevent.duration},
         |  ${flatVevent.attach},
         |  ${flatVevent.attendee},
         |  ${flatVevent.categories},
         |  ${flatVevent.comment},
         |  ${flatVevent.contact},
         |  ${flatVevent.exdate},
         |  ${flatVevent.rstatus},
         |  ${flatVevent.related},
         |  ${flatVevent.resources},
         |  ${flatVevent.rdate},
         |  ${flatVevent.xProp},
         |  ${flatVevent.ianaProp}
         |)
       """.stripMargin.update.run.transact(xa).unsafeRunSync
  }

  def extractValueFromGroupA(elem: Option[GroupA]): String = {
    elem match {
      case Some(e) => e.value
      case _ => ""
    }
  }

  def extractValueFromGroupB(elem: Option[GroupB]): String = {
    elem match {
      case Some(e) => e.value
      case _ => ""
    }
  }

  def extractValueFromGroupC(elem: Option[GroupC]): String = {
    elem match {
      case Some(e) => e.value
      case _ => ""
    }
  }

  def extractValueFromGroupD(elem: Option[GroupD]): String = {
    elem match {
      case Some(e) => e.value
      case _ => ""
    }
  }

}
