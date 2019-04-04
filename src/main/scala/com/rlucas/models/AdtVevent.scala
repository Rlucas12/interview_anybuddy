package com.rlucas.models

import com.rlucas.models.ICalendar._

case class FlatVevent(
 dtstamp: String,
 uid: String,
 dtstart: String,
 `class`: String,
 created: String,
 description: String,
 geo: String,
 lastMod: String,
 location: String,
 organizer: String,
 priority: String,
 seq: String,
 status: String,
 summary: String,
 transp: String,
 url: String,
 recurid: String,
 rrule: String,
 dtend: String,
 duration: String,
 attach: String,
 attendee: String,
 categories: String,
 comment: String,
 contact: String,
 exdate: String,
 rstatus: String,
 related: String,
 resources: String,
 rdate: String,
 xProp: String,
 ianaProp: String
)
case class AdtVevent(
  groupA: Seq[GroupA],
  dtStart: Seq[Dtstart],
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

  def adtVeventtoFlat(vevent: AdtVevent): FlatVevent = {
    FlatVevent(
      extractValueFromGroupA(vevent.groupA.find(_.isInstanceOf[Dstamp])),
      extractValueFromGroupA(vevent.groupA.find(_.isInstanceOf[Uid])),
      extractValueFromDtstart(vevent.dtStart.find(_.isInstanceOf[Dtstart])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[ClassProp])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Created])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Description])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Geo])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[LastMod])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Location])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Organizer])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Priority])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[SeqProp])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Status])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Summary])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Transp])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Url])),
      extractValueFromGroupB(vevent.groupB.find(_.isInstanceOf[Recurid])),
      extractValueFromRrule(vevent.rrule.find(_.isInstanceOf[Rrule])),
      extractValueFromGroupC(vevent.groupC.find(_.isInstanceOf[Dtend])),
      extractValueFromGroupC(vevent.groupC.find(_.isInstanceOf[Duration])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Attach])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Attendee])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Categories])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Comment])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Contract])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Exdate])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Rstatus])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Related])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Resources])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Rdate])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[Xprop])),
      extractValueFromGroupD(vevent.groupD.find(_.isInstanceOf[IanaProp]))
    )
  }

  def extractValueFromGroupA(elem: Option[GroupA]): String = {
    elem match {
      case Some(e) => e.value
      case _ => ""
    }
  }

  def extractValueFromDtstart(elem: Option[Dtstart]): String = {
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

  def extractValueFromRrule(elem: Option[Rrule]): String = {
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
