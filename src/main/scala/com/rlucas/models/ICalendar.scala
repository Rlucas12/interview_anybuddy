package models

trait TypeProp {
 val value: String
}

trait GroupA extends TypeProp
case class Dtstamp(value: String) extends GroupA
case class Uid(value: String) extends GroupA

case class Dtstart(value: String) extends TypeProp

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
case class Dtend(value: String) extends GroupC
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

case class Other(value: String)

case class AdtVevent(groupA: Seq[GroupA], dtStart: Option[TypeProp], groupB: Seq[GroupB], rrule: Rrule, groupC: GroupC, groupD: GroupD)
case class FullAdt(others: Seq[Other], adtVevent: AdtVevent)
