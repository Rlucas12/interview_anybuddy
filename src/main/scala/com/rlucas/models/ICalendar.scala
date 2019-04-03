package com.rlucas.models

trait ICalendar {

 trait TypeProp {
  val value: String
 }

 trait GroupA extends TypeProp
 case class Dstamp(value: String) extends GroupA
 case class Uid(value: String) extends GroupA

 case class Dtstart(value: String, argument: String) extends TypeProp

 trait GroupB extends TypeProp
 case class ClassProp(value: String) extends GroupB
 case class Created(value: String) extends GroupB
 case class Description(value: String) extends GroupB
 case class Geo(value: String) extends GroupB
 case class LastMod(value: String) extends GroupB
 case class Location(value: String) extends GroupB
 case class Organizer(value: String) extends GroupB
 case class Priority(value: String) extends GroupB
 case class SeqProp(value: String) extends GroupB
 case class Status(value: String) extends GroupB
 case class Summary(value: String) extends GroupB
 case class Transp(value: String) extends GroupB
 case class Url(value: String) extends GroupB
 case class Recurid(value: String) extends GroupB

 case class Rrule(value: String) extends TypeProp

 trait GroupC extends TypeProp
 case class Dtend(value: String, argument: String) extends GroupC
 case class Duration(value: String) extends GroupC

 trait GroupD extends TypeProp
 case class Attach(value: String) extends GroupD
 case class Attendee(value: String) extends GroupD
 case class Categories(value: String) extends GroupD
 case class Comment(value: String) extends GroupD
 case class Contract(value: String) extends GroupD
 case class Exdate(value: String) extends GroupD
 case class Rstatus(value: String) extends GroupD
 case class Related(value: String) extends GroupD
 case class Resources(value: String) extends GroupD
 case class Rdate(value: String) extends GroupD
 case class Xprop(value: String) extends GroupD
 case class IanaProp(value: String) extends GroupD

 case class Other(value: String) extends TypeProp

 case class AdtVevent(groupA: Seq[GroupA], dtStart: Option[TypeProp], groupB: Seq[GroupB])
 case class FullAdt(others: Seq[Other], adtVevent: AdtVevent)

 val txt = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTSTART;VALUE=DATE:20190403\nDTEND;VALUE=DATE:20190404\nDTSTAMP:20190403T111803Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T103218Z\nLOCATION:\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nX-APPLE-TRAVEL-ADVISORY-BEHAVIOR:AUTOMATIC\nBEGIN:VALARM\nACTION:AUDIO\nTRIGGER:-PT15H\nX-WR-ALARMUID:CAD690A6-EDB9-4177-BE07-D60AE4F533CD\nUID:CAD690A6-EDB9-4177-BE07-D60AE4F533CD\nX-APPLE-DEFAULT-ALARM:TRUE\nATTACH;VALUE=URI:Basso\nEND:VALARM\nEND:VEVENT\nEND:VCALENDAR"

 def rowToProp(row: String): TypeProp = {
  val patternWithoutArgument = s"""(.*):(.*)""".r
  val patternWithArgument = s"""(.*?);(.*):(.*)""".r

  row match {
   case patternWithoutArgument("CLASS",value) => ClassProp(value)
   case patternWithoutArgument("DTSTAMP", value) => Dstamp(value)
   case patternWithoutArgument("UID" , value) => Uid(value)
   case patternWithArgument("DTSTART" , arg, value) => Dtstart(value, arg)
   case patternWithoutArgument("DESCRIPTION", value) => Description(value)
   case patternWithoutArgument("GEO", value) => Geo(value)
   case patternWithoutArgument("LASTMOD", value) => LastMod(value)
   case patternWithoutArgument("LOCATION", value) => Location(value)
   case patternWithoutArgument("ORGANIZER", value) => Organizer(value)
   case patternWithoutArgument("PRIORITY", value) => Priority(value)
   case patternWithoutArgument("SEQ", value) => SeqProp(value)
   case patternWithoutArgument("STATUS", value) => Status(value)
   case patternWithoutArgument("SUMMARY", value) => Summary(value)
   case patternWithoutArgument("TRANSP", value) => Transp(value)
   case patternWithoutArgument("URL", value) => Url(value)
   case patternWithoutArgument("RECURID", value) => Recurid(value)
   case patternWithoutArgument("RRULE", value) => Rrule(value)
   case patternWithArgument("DTEND", arg, value) => Dtend(value, arg)
   case patternWithoutArgument("DURATION",value) => Duration(value)
   case patternWithoutArgument("ATTACH", value) => Attach(value)
   case patternWithoutArgument("ATTENDEE", value) => Attendee(value)
   case patternWithoutArgument("CATEGORIES", value) => Categories(value)
   case patternWithoutArgument("COMMENT", value) => Comment(value)
   case patternWithoutArgument("CONTRACT", value) => Contract(value)
   case patternWithoutArgument("RSTATUS", value) => Rstatus(value)
   case patternWithoutArgument("RELATED", value) => Related(value)
   case patternWithoutArgument("RESOURCES", value) => Resources(value)
   case patternWithoutArgument("RDATE", value) => Rdate(value)
   case patternWithoutArgument("XPROP", value) => Xprop(value)
   case patternWithoutArgument("IANAPROP", value) => IanaProp(value)
   case other => Other(other)
  }
 }

 def parseVevent(rows: List[String], acc: List[TypeProp]): Either[String, List[TypeProp]] = {
  rows match {
   case Nil => Left("Unable to parse VEVENT")
   case "END:VEVENT" :: tail => Right(acc)
   case row :: tail => parseVevent(tail, acc :+ rowToProp(row))
  }
 }

 def parseCalendar(rows: List[String], methodDetected: Boolean): Either[String, List[TypeProp]] = {
  val methodPattern = """METHOD:(.*)""".r
  rows match {
   case Nil => Left("empty")
   case "BEGIN:VEVENT"   :: tail => parseVevent(tail, Nil)
   case methodPattern(_) :: tail => parseCalendar(tail, true)
   case hd :: tail => parseCalendar(tail, methodDetected)
  }
 }

}
