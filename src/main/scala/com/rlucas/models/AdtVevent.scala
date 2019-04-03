package com.rlucas.models

import com.rlucas.models.ICalendar._

case class AdtVevent(
  groupA: Seq[GroupA],
  dtStart: Seq[TypeProp],
  groupB: Seq[GroupB],
  rrule: Seq[Rrule],
  groupC: Seq[GroupC],
  groupD: Seq[GroupD],
  others: Seq[Other],
  method: Boolean
)

object AdtVevent {
  import cats.implicits._

  def empty: AdtVevent = AdtVevent(Nil, Nil, Nil, Nil, Nil, Nil, Nil, false)

  def validate(adt: AdtVevent): Either[String, AdtVevent] = {
    checkGroupA(adt) *> checkDtStart(adt) *> checkGroupB(adt) *> checkRrule(adt) *> checkGroupC(adt) *> checkGroupD(adt)
  }

  def checkGroupA(adt: AdtVevent): Either[String, AdtVevent] = {
    adt.groupA match {
      case (_: Dstamp) :: (_: Uid)  :: Nil => Right(adt)
      case (_: Uid) :: (_: Dstamp)  :: Nil => Right(adt)
      case _ => Left("REQUIRED, UID & DSTAMP but MUST NOT occur more than once")
    }
  }

  def checkDtStart(adt: AdtVevent): Either[String, AdtVevent] = {
    (adt.dtStart.size, adt.method) match {
      case (1, false) => Right(adt)
      case (0, false) => Left("DTSTART not defined but METHOD property not defined")
      case (1, true) => Right(adt)
      case _ => Left("DTSTART occurred most once")
    }
  }

  def checkGroupB(adt: AdtVevent): Either[String, AdtVevent] = {
    val occurences = adt.groupB.groupBy(_.getClass.getCanonicalName).filter {
      case (_, _ :: _ :: Nil) => true
      case _ => false
    }

    occurences.size match {
      case 0 => Right(adt)
      case _ => Left(
        """class / created / description / geo /
          |last-mod / location / organizer / priority / seq /
          |status / summary / transp / url / recurid /
          | SHOULD NOT occur more than once
          |""".stripMargin)
    }
  }

  def checkRrule(adt: AdtVevent): Either[String, AdtVevent] = {
    Either.cond(adt.rrule.size <= 1, adt, "RRULE SHOULD NOT occur more than once")
  }

  def checkGroupC(adt: AdtVevent): Either[String, AdtVevent] = {
    val numberDtEnd = adt.groupC.filter {
      case _: Dtend => true
      case _ => false
    }.size

    val numberDuration = adt.groupC.filter {
      case _: Dtend => false
      case _ => true
    }.size

    (numberDtEnd, numberDuration) match {
      case (0, _) => Right(adt)
      case (_, 0) => Right(adt)
      case _ => Left(
        """Either 'dtend' or 'duration'
          |MAY appear in ; a 'eventprop', but 'dtend' and
          |'duration' MUST NOT occur in the same 'eventprop'""".stripMargin)
    }
  }

  def checkGroupD(adt: AdtVevent): Either[String, AdtVevent] = {
    val occurences = adt.groupB.groupBy(_.getClass.getCanonicalName).filter {
      case (_, _ :: _ :: Nil) => true
      case _ => false
    }

    occurences.size match {
      case 0 => Right(adt)
      case _ => Left(
        """attach / attendee / categories / comment /
          | contact / exdate / rstatus / related /
          | resources / rdate / x-prop / iana-prop""".stripMargin)
    }
  }
}
