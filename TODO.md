* Run as an sbt app with tab completions
* Configurable summarisers. List in application.conf with filter like dictionaries. tyro-lagotto
 - 3120 status advices can show last message
 - Use log-tools as basis
* Detail for --ui should show names by default with links to raw (existing and digest)
 - need to send text/html instead of text/plain and escape XML
 - maybe syntax highlight
* lock free Single writer, single reader structure for queue of futures - mechanical sympathy?
  - http://psy-lob-saw.blogspot.com.au/2013/03/single-producerconsumer-lock-free-queue.html
* log time for each file perf mode? 
* --ui should serve .svg with right tz for mouse over, bar chart option
* --in-format json
* can options be made saner
 - output formatting
  - style: raw, named, digest, tabular (csv, tsv, html, jira, ascii, utf), json
  - text: raw vs named
  - digest: with and without names (different length of names)
  - colour (ansi vs html)
  - type of name
  orthogonal
  --names (only valid with text and digest? maybe json)
  --color (automatically use ansi for console or html with --ui)
  
  -t time,message -o ascii
* colour log4j and simple  
* Time conversion operator. Eg parse(12,yyyyMMddHHmmss) for 20141218151240. Then allow use in convert() for math?
* Hint about reading stdin if no input after 0.5 second?
* Multi-project with spark and sbt-io examples as children?
* Apache Spark module
* HTML table
  - colour rows by a field like socket
  - colour rows by match condition like lifespan>1000
  - click socket to filter by socket
* Output as something readable by PCP (http://www.pcp.io/docs/pcpintro.html)?
* Build an index and/or process data into more compact/faster to re-read format
 - might ease path to Spark
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
* URL schemes for file names. More interesting with -F
 - ssh://user@host/path
 - http(s)://
* Filter before and after --join. At least work out when a filter must be after --join
* Handle FSD log messages?
* Implement google datasource for charting API https://developers.google.com/chart/interactive/docs/dev/dsl_get_started
* Why does spark sql not see export json as having date type for timestamps?
* Support "receiver -> sender" display. Name system where logs are gathered from.
 - web sequence diagram output...
* Full SQL like query: and, or - parser combinator
* Warn if pivot on field is not sorted. Pivot assumes entries to be pivoted are consecutive

group_trace() should bound number of files kept open to stay under ulimit. Reopen if required.
dictionary should have 43.1 as "merchant name" for 0200,0100,0400

window function? http://www.postgresql.org/docs/9.4/interactive/tutorial-window.html

Politeness:
* Warn if sorting lots of rows: gonna die due to GC
* Warn about typos like count(exception)
* (max(lifespan as time(s)),(min(lifespan as time(s))) is not going to return anything. Did you mean (max(lifespan) as time(s))
* Show progress by default if output is batched (not incremental). Disable progress with --no-progress?

* Deploy artifact to maven central with fat .jar attachment
* bintray/sonatype snapshots for artifact hosting?
* List available fields in --help
 - mention numeric comparison for >,< with fallback to string
 - count - like uniq -c
 - count(condition) like count(rtt>100) 
 - concat like mysql's group_concat
 - min,max,sum (maybe useless)
 - time with format - Eg {HH:mm} plus HH:m0 and HH:mm:s0
 - regex mode is partial match by default (link to javadoc)
 - can filter on calc() expressions like calc(max(time)-min(time))>N
 - Dictionary names can be used in queries too. Eg -f deviceId=42abcd where deviceId maps to 48.x or 48.y depending on (MTI,NMIC)
* Document repeating groups in dictionary: Eg 48.{1..10} = "Blah"

Add examples
- Output expression field(/regex/$1/)
- filter regex for ~//

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
* https://github.com/trifacta/vega/wiki/Tutorial

* use parbolied https://github.com/sirthias/parboiled or built in parser combinators for parsing expressions?
* integrated with beaker ? https://github.com/twosigma/beaker-notebook/wiki/Create-an-OutputDisplay
* publish to m2 repos https://github.com/xerial/sbt-sonatype
