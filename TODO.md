* Should grep be case insensitive?
* Translate field numbers into names on output
 - needs a data dictionary. perhaps use ISO 8583 defaults (public spec?)
* Dictionary could be used in queries too. Eg -f deviceId=42abcd where deviceId maps to 48.x or 48.y depending on (MTI,NMIC)
* Tail mode (-F)
* Hint about reading stdin if no input after 0.5 second?
* Multi-project with spark and sbt-io examples as children?
* Add --splitBy field to Main? Use case would be ip address or transaction id or link
 - using scalaz streams?
* JSON output format
* Output to websocket and/or server sent events subscribers?
* Apache Spark module
* Output with gnuplot header?
* Can LogFieldExpr allow access to more specific type
 * Override toXsv() on LogEntry?
* Output as
 - HTML table
  - colour rows by a field like socket
  - colour rows by match condition like lifespan>1000
* More icons: session start and end, error
* Add row number field
* Value in multiple threads? actually CPU bound
* Remove unicode dog. Not well supported...
* Output as something readable by PCP (http://www.pcp.io/docs/pcpintro.html)?
* Ascii bar chart
* Validate only one of delay or count is passed
* Build an index and/or process data into more compact/faster to re-read format
 - might ease path to Spark
* Can --pair idea by expanded to a SQL like join. One case for keeping memoized streams...
* Graceful error reporting...
* Other input log formats: log4j, httpd
* calc(timestamp-lifespan)
* Allow aggregation like min(calc(timestamp-lifespan))

Politeness:
* Warn if sorting lots of rows: gonna die due to GC
* Warn about typos like count(exception)

Bugs:
 * Same for something like count(time(mm:ss)=00:03)>N (aggregates)

* Deploy artifact to maven central with fat .jar attachment
* Codeship.io free account
* bintray/sonatype snapshots for artifact hosting?
* List available fields in --help
 - mention regex for ~//
 - mention numeric comparison for >,< with fallback to string
 - rtt
 - delay
 - count - like uniq -c
 - count(condition) like count(rtt>100)
 - concat like mysql's group_concat
 - min,max,sum (maybe useless)
 - time with format - Eg {HH:mm} plus HH:m0 and HH:mm:s0
 - regex mode is partial match by default (link to javadoc)
 - field(/regex/$1/)
 - can filter on calc() expressions like calc(max(time)-min(time))>N

use cases:
* fraction of auths as total messages calc(count(mti=0200)/count)
* rtt per outgoing link
* send time per client
* max concurrent requests?
* stalls -> max delay between messages
* throughput: incoming/sec outgoing/sec
* HDR histogram: use cases other than rtt and lifespan?