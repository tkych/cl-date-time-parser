Last modified : 2013-07-25 23:23:58 tkych

version 0.1.00 (beta)


CL-Date-Time-Parser
===================


> In general, an implementation should be conservative in its sending
> behavior, and liberal in its receiving behavior.
> [RFC791: Internet Protocol](http://tools.ietf.org/html/rfc791)

There are several formats for specifying date and time on the Web.
For example, "Thu, 23 Jul 2013 19:42:23 JST" (RFC822),
"2013-07-23T19:42:23+09:00" (ISO8601), etc.

The goal of cl-date-time-parser is to hide the difference between
date-time formats, and enable to manage date and time as the one date
time format (Universal Time).

Function `parse-date-time` parses date-time-string, and return
universal-time and fraction.  Parsable date time formats are RFC822
(RFC1123, RFC2822, RFC5322), asctime, ISO8601(1988, 2000, 2004, except
for without year), W3CDTF and RFC3339.
`parse-date-time` can liberally parse the above formats with a little
broken.


Examples
--------

    (date-time-parser:parse-date-time "Thu, 23 Jul 2013 19:42:23 JST")
    => 3583564943, 0

    (date-time-parser:parse-date-time "2013-07-23T19:42:23+09:00")
    => 3583564943, 0

    (date-time-parser:parse-date-time "23 Jul 13 19:42:23 +0900")
    => 3583564943, 0

    (date-time-parser:parse-date-time "Thu Jul 23 19:42:23 JST 2013")
    => 3583564943, 0

    (date-time-parser:parse-date-time "2013-07-23T19:42:23.45Z")
    => 3583597343, 0.45

    (date-time-parser:parse-date-time "2013-01-01")
    => 3565987200, 0

    (date-time-parser:parse-date-time "2013")
    => 3565987200, 0

    (date-time-parser:parse-date-time "1 Jan 13")
    => 3565987200, 0

    (date-time-parser:parse-date-time "2003-12-31T25:14:55Z") ;broken hours
    => 3281908495, 0
    (date-time-parser:parse-date-time "2004-01-01T01:14:55Z")
    => 3281908495, 0

    (date-time-parser:parse-date-time "2003-12-31T10:61:55Z") ;broken minuits
    => 3281857315, 0
    (date-time-parser:parse-date-time "2003-12-31T11:01:55Z")
    => 3281857315, 0

    (date-time-parser:parse-date-time "2003-12-31T10:14:61Z") ;broken seconds
    => 3281854501, 0
    (date-time-parser:parse-date-time "2003-12-31T10:15:01Z")
    => 3281854501, 0


For further examples, please see Eval-Test in date-time-parser.lisp


Depends-on
----------

 * [alexandria](http://common-lisp.net/project/alexandria/)
 * [anaphora](http://common-lisp.net/project/anaphora/)
 * [split-sequence](http://www.cliki.net/split-sequence)
 * [cl-ppcre](http://weitz.de/cl-ppcre/)
 * [local-time](http://common-lisp.net/project/local-time/)
 * [parse-float](https://github.com/soemraws/parse-float)


Installation
------------

 0. SHELL$   `git clone https://github.com/tkych/cl-date-time-parser.git`
 1. CL-REPL> `(push #p"/path-to-cl-date-time-parser/cl-date-time-parser/" asdf:*central-registry*)`
 2. CL-REPL> `(ql:quickload :cl-date-time-parser)` or `(asdf:load-system :cl-date-time-parser)`


Manual
------

#### [Function] PARSE-DATE-TIME _date-time-string_ => _universal-time_, _fraction_

Parse _date-time-string_, and return _universal-time_ and _fraction_.
_date-time-string_ must represent the date-time after 1900-01-01T00:00:00Z.

Parsable Formats:

 * RFC822 Genus: RFC822, RFC1123, RFC2822, RFC5322, asctime.
 * ISO8601 Genus: ISO8601(:1988, :2000 and :2004. except for no-year), W3CDTF, RFC3339.
 * Broken format: The above formats with little broken.


Reference
---------

 * RFC822 (Internet Message Format) Genus:
   * [RFC822: Standard for the Format of Arpa Internet Text Messages](http://tools.ietf.org/html/rfc822)
   * [RFC1123: Requirements for Internet Hosts -- Application and Support](http://tools.ietf.org/html/rfc1123)
   * [RFC2822: Internet Message Format](http://tools.ietf.org/html/rfc2822)
   * [RFC5322: Internet Message Format](http://tools.ietf.org/html/rfc5322)
     * [errata for RFC5322](http://www.rfc-editor.org/errata_search.php?rfc=5322)
   * [asctime format](http://en.cppreference.com/w/c/chrono/asctime)

 * ISO8601 (Timestamp) Genus:
   * [ISO8601:1988, 2000, 2004](http://www.iso.org/iso/home/standards/iso8601.htm)
   * [W3CDTF: Date and Time Formats](http://www.w3.org/TR/1998/NOTE-datetime-19980827)
   * [RFC3339: Date and Time on the Internet: Timestamps](http://tools.ietf.org/html/rfc3339)
     * [errata for RFC3339](http://www.rfc-editor.org/errata_search.php?rfc=3339)


Author, License, Copyright
--------------------------

 - Takaya OCHIAI  <#.(reverse "moc.liamg@lper.hcykt")>

 - MIT License

 - Copyright (C) 2013 Takaya OCHIAI
