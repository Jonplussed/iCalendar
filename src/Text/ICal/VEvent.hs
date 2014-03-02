module VEvent (vEvent) where

{--
  A "VEVENT" calendar component is defined by the
  following notation:

    eventc     = "BEGIN" ":" "VEVENT" CRLF
                 eventprop *alarmc
                 "END" ":" "VEVENT" CRLF

    eventprop  = *(

               ; the following are optional,
               ; but MUST NOT occur more than once

               class / created / description / dtstart / geo /
               last-mod / location / organizer / priority /
               dtstamp / seq / status / summary / transp /
               uid / url / recurid /

               ; either 'dtend' or 'duration' may appear in
               ; a 'eventprop', but 'dtend' and 'duration'
               ; MUST NOT occur in the same 'eventprop'

               dtend / duration /

               ; the following are optional,
               ; and MAY occur more than once

               attach / attendee / categories / comment /
               contact / exdate / exrule / rstatus / related /
               resources / rdate / rrule / x-prop

               )
--}

{--
  An example iCalendar containing a single VEVENT:

  BEGIN:VCALENDAR
  VERSION:2.0
  PRODID:-//hacksw/handcal//NONSGML v1.0//EN
  BEGIN:VEVENT
  UID:uid1@example.com
  DTSTAMP:19970714T170000Z
  ORGANIZER;CN=John Doe:MAILTO:john.doe@example.com
  DTSTART:19970714T170000Z
  DTEND:19970715T035959Z
  SUMMARY:Bastille Day Party
  END:VEVENT
  END:VCALENDAR
--}

{--
  The type of VEVENT I'm trying to parse:

  BEGIN:VEVENT
  UID:111-11583@uwbadgers.com
  DTSTART:20131123T203000Z
  DURATION:PT3H
  ATTENDEE:no-reply@uwbadgers.com
  ORGANIZER:no-reply@uwbadgers.com
  LOCATION:Minneapolis, Minn.
  SUMMARY:Football: Wisconsin at Minnesota
  DESCRIPTION:Media:\nESPN
  TRANSP:TRANSPARENT
  END:VEVENT
--}


