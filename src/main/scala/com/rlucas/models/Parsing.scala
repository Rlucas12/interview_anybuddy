package com.rlucas.models

import com.rlucas.models.ICalendar._

object Parsing {

  def rowToProp(row: String, adtVevent: AdtVevent): AdtVevent = {
    val patternWithoutArgument = s"""(.*):(.*)""".r
    val patternWithArgument = s"""(.*?);(.*):(.*)""".r

    row match {
      case patternWithoutArgument("CLASS",value) => adtVevent.copy(groupB = (adtVevent.groupB :+ ClassProp(value)))
      case patternWithoutArgument("DTSTAMP", value) => adtVevent.copy(groupA = (adtVevent.groupA :+ Dstamp(value)))
      case patternWithoutArgument("UID" , value) => adtVevent.copy(groupA = (adtVevent.groupA :+ Uid(value)))
      case patternWithArgument("DTSTART" , arg, value) => adtVevent.copy(dtStart = (adtVevent.dtStart :+ Dtstart(value, arg)))
      case patternWithoutArgument("DESCRIPTION", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Description(value)))
      case patternWithoutArgument("GEO", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Geo(value)))
      case patternWithoutArgument("LASTMOD", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ LastMod(value)))
      case patternWithoutArgument("LOCATION", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Location(value)))
      case patternWithoutArgument("ORGANIZER", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Organizer(value)))
      case patternWithoutArgument("PRIORITY", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Priority(value)))
      case patternWithoutArgument("SEQ", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ SeqProp(value)))
      case patternWithoutArgument("STATUS", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Status(value)))
      case patternWithoutArgument("SUMMARY", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Summary(value)))
      case patternWithoutArgument("TRANSP", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Transp(value)))
      case patternWithoutArgument("URL", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Url(value)))
      case patternWithoutArgument("RECURID", value) => adtVevent.copy(groupB = (adtVevent.groupB :+ Recurid(value)))
      case patternWithoutArgument("RRULE", value) => adtVevent.copy(rrule = (adtVevent.rrule :+ Rrule(value)))
      case patternWithArgument("DTEND", arg, value) => adtVevent.copy(groupC = (adtVevent.groupC :+ Dtend(value, arg)))
      case patternWithoutArgument("DURATION",value) => adtVevent.copy(groupC = (adtVevent.groupC :+ Duration(value)))
      case patternWithoutArgument("ATTACH", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Attach(value)))
      case patternWithoutArgument("ATTENDEE", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Attendee(value)))
      case patternWithoutArgument("CATEGORIES", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Categories(value)))
      case patternWithoutArgument("COMMENT", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Comment(value)))
      case patternWithoutArgument("CONTRACT", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Contract(value)))
      case patternWithoutArgument("RSTATUS", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Rstatus(value)))
      case patternWithoutArgument("RELATED", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Related(value)))
      case patternWithoutArgument("RESOURCES", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Resources(value)))
      case patternWithoutArgument("RDATE", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Rdate(value)))
      case patternWithoutArgument("XPROP", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ Xprop(value)))
      case patternWithoutArgument("IANAPROP", value) => adtVevent.copy(groupD = (adtVevent.groupD :+ IanaProp(value)))
      case other => adtVevent.copy(others = (adtVevent.others :+ Other(other)))
    }
  }

  def skipOtherValarm(rows: List[String]): Either[String, List[String]] = {
    rows match {
      case "BEGIN:VALARM" :: tail => skipOtherValarm(tail)
      case "END:VALARM" :: tail => Right(tail)
      case  Nil => Left("Unable to parse VEVENT, VALARM malformed")
      case  _ :: tail => skipOtherValarm(tail)
    }
  }

  def parseVevent(rows: List[String], adt: AdtVevent): Either[String, AdtVevent] = {
    rows match {
      case Nil => Left("Unable to parse VEVENT")
      case "END:VEVENT" :: _ => Right(adt)
      case "BEGIN:VALARM" :: tail => skipOtherValarm(tail).flatMap(rows => parseVevent(rows, adt))
      case row :: tail => parseVevent(tail, rowToProp(row, adt))
    }
  }

  def parseCalendar(rows: List[String], methodDetected: Boolean): Either[String, AdtVevent] = {
    val methodPattern = """METHOD:(.*)""".r

    rows match {
      case Nil => Left("empty")
      case "BEGIN:VEVENT"   :: tail => parseVevent(tail, AdtVevent.empty).flatMap(AdtVevent.validate)
      case methodPattern(_) :: tail => parseCalendar(tail, true)
      case _ :: tail => parseCalendar(tail, methodDetected)
    }
  }

}
