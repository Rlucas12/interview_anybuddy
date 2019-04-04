import com.rlucas.models.ICalendar._
import com.rlucas.models._
import com.rlucas.models.Parsing.parseCalendar
import org.scalatest.FlatSpec

class VeventSpec extends FlatSpec {

  "Vevent spec" should "passed" in {
      val cal = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTSTART;VALUE=DATE:20190403\nDTEND;VALUE=DATE:20190404\nDTSTAMP:20190403T103214Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T085647Z\nLOCATION:\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nEND:VEVENT\nEND:VCALENDAR"
      val rows: List[String] = cal.split('\n').map(_.trim.filter(_ >= ' ')).toList
      val response = parseCalendar(rows, false)

      assert(response === Right(AdtVevent(List(Dstamp("20190403T103214Z"), Uid("31h42d21eva7s6aaaunkqqklml@google.com")),List(Dtstart("20190403","VALUE=DATE")),List(Description(""), Location(""), Status("CONFIRMED"), Summary("Test event"), Transp("TRANSPARENT")),List(),List(Dtend("20190404","VALUE=DATE")),List(),List(Other("CREATED:20190403T085647Z"), Other("LAST-MODIFIED:20190403T085647Z"), Other("SEQUENCE:0")),false)))
  }

  "Vevent spec" should "failed because wrong GroupA validation" in {
    val cal = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTSTART;VALUE=DATE:20190403\nDTEND;VALUE=DATE:20190404\nDTSTAMP:20190403T103214Z\nDTSTAMP:20190403T103214Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T085647Z\nLOCATION:\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nEND:VEVENT\nEND:VCALENDAR"
    val rows: List[String] = cal.split('\n').map(_.trim.filter(_ >= ' ')).toList
    val response = parseCalendar(rows, false)

    assert(response === Left("REQUIRED, UID & DSTAMP but MUST NOT occur more than once"))
  }

  "Vevent spec" should "failed because wrong dtstart validation" in {
    val cal = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTEND;VALUE=DATE:20190404\nDTSTAMP:20190403T103214Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T085647Z\nLOCATION:\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nEND:VEVENT\nEND:VCALENDAR"
    val rows: List[String] = cal.split('\n').map(_.trim.filter(_ >= ' ')).toList
    val response = parseCalendar(rows, false)

    assert(response === Left("DTSTART not defined but METHOD property not defined"))
  }

  "Vevent spec" should "failed because wrong GroupB validation" in {
    val cal = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTSTART;VALUE=DATE:20190403\nDTEND;VALUE=DATE:20190404\nDTSTAMP:20190403T103214Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T085647Z\nLOCATION:\nSEQUENCE:0\nSTATUS:CONFIRMED\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nEND:VEVENT\nEND:VCALENDAR"
    val rows: List[String] = cal.split('\n').map(_.trim.filter(_ >= ' ')).toList
    val response = parseCalendar(rows, false)

    assert(response === Left(
      """class / created / description / geo /
        |last-mod / location / organizer / priority / seq /
        |status / summary / transp / url / recurid /
        | SHOULD NOT occur more than once
        |""".stripMargin))
  }

  "Vevent spec" should "failed because wrong rrule validation" in {
    val cal = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTSTART;VALUE=DATE:20190403\nDTEND;VALUE=DATE:20190404\nDTSTAMP:20190403T103214Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T085647Z\nLOCATION:\nRRULE:2\nRRULE:2\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nEND:VEVENT\nEND:VCALENDAR"
    val rows: List[String] = cal.split('\n').map(_.trim.filter(_ >= ' ')).toList
    val response = parseCalendar(rows, false)

    assert(response === Left("RRULE SHOULD NOT occur more than once"))
  }

  "Vevent spec" should "failed because wrong GroupC validation" in {
    val cal = "BEGIN:VCALENDAR\nPRODID:-//Google Inc//Google Calendar 70.9054//EN\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Interview Anybuddy\nX-WR-TIMEZONE:Europe/Paris\nX-WR-CALDESC:Interview Anybuddy project\nBEGIN:VEVENT\nDTSTART;VALUE=DATE:20190403\nDTEND;VALUE=DATE:20190404\nDURATION:2\nDTSTAMP:20190403T103214Z\nUID:31h42d21eva7s6aaaunkqqklml@google.com\nCREATED:20190403T085647Z\nDESCRIPTION:\nLAST-MODIFIED:20190403T085647Z\nLOCATION:\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:Test event\nTRANSP:TRANSPARENT\nEND:VEVENT\nEND:VCALENDAR"
    val rows: List[String] = cal.split('\n').map(_.trim.filter(_ >= ' ')).toList
    val response = parseCalendar(rows, false)

    assert(response === Left(
      """Either 'dtend' or 'duration'
        |MAY appear in ; a 'eventprop', but 'dtend' and
        |'duration' MUST NOT occur in the same 'eventprop'""".stripMargin))
  }
}
