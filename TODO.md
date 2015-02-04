* Time conversion operator. Eg parse(12,yyyyMMddHHmmss) for 20141218151240. Then allow use in convert() for math?
* Type dictionary/schema?
 - mti, timestamp (with format), number
 - 11,lifespan,rtt,delay = number, timestamp,7 = datetime
 - Having the datatype propagated into Spark would max it easier to do things like max(lifespan) there
* Tail mode (-F)
* Hint about reading stdin if no input after 0.5 second?
* Multi-project with spark and sbt-io examples as children?
* Add --splitBy field to Main? Use case would be ip address or transaction id or link
 - using scalaz streams?
* Output to websocket and/or server sent events subscribers?
* Apache Spark module
* Output as
 - HTML table
  - colour rows by a field like socket
  - colour rows by match condition like lifespan>1000
* More icons: session start and end, error
* Add row number field
* Value in multiple threads? actually CPU bound
* Output as something readable by PCP (http://www.pcp.io/docs/pcpintro.html)?
* Ascii bar chart
* Validate only one of delay or count is passed
* Build an index and/or process data into more compact/faster to re-read format
 - might ease path to Spark
* Can --pair idea by expanded to a SQL like join. One case for keeping memoized streams...
* Graceful error reporting...
* Split some pieces like ascii table and gnuplot out. Could run independently.
 - Eg csv -> ascii, csv -> gnuplot per column, csv -> jira table, csv -> html
* Would interning some strings help performance?
* Convert part of jpos CMF into dictionary: https://github.com/jpos/jPOS-CMF/blob/master/src/docx/result_codes.xml
* Allow short names everywhere a PrimitiveExpr is being used. Eg
 group_concat(distinct(translate(mti)))
* Deep custom dictionaries have pathetic performance
* Channel tagging: rewrite <log> with extra attribute based on data sent in handshake message
* Remove fields vs subfields distinction from dictionaries
* Dump dictionary as a table for sane MTI-NMIC combinations
* Add alias like SQL expr as 'sane name'
* Remote -persist from gnuplot
 for i in {1..4}; do echo "switch $i"; lago se$i*.gz --in-format gc -t datetime,delay,pause --gnuplot $i-gc ; done
* Make xpath() a FieldExpr. Avoids need to cache expressions
* Pair on ip,mti,nmic,time threshold?
* Can --progress estimate end time with one large file?

Politeness:
* Warn if sorting lots of rows: gonna die due to GC
* Warn about typos like count(exception)
* (max(lifespan as time(s)),(min(lifespan as time(s))) is not going to return anything. Did you mean (max(lifespan) as time(s))
* Show progress by default if output is batched (not incremental). Disable progress with --no-progress?

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
 - Dictionary names can be used in queries too. Eg -f deviceId=42abcd where deviceId maps to 48.x or 48.y depending on (MTI,NMIC)
* Document repeating groups in dictionary: Eg 48.{1..10} = "Blah"

use cases:
* fraction of auths as total messages calc(count(mti=0200)/count)
* rtt per outgoing link
* send time per client
* max concurrent requests?
* stalls -> max delay between messages
* throughput: incoming/sec outgoing/sec
* HDR histogram: use cases other than rtt and lifespan?

spark:
* Add header row to ascii table from schema
* Expose other table formats: xsv, jira, html, hdr histogram, gnuplot?
* Publish UI from shell to something like Zeppelin. Means you still have tab completion. Why no tab completion in zeppelin?
 - Could use schema.toJSON

charting options:
* http://code.shutterstock.com/rickshaw/
* http://beakernotebook.com/
* https://github.com/andypetrella/spark-notebook

* use parbolied https://github.com/sirthias/parboiled or built in parser combinators for parsing expressions?
* integrated with beaker ? https://github.com/twosigma/beaker-notebook/wiki/Create-an-OutputDisplay
* publish to m2 repos https://github.com/xerial/sbt-sonatype
