package net.szachcial.q4l2

import util.parsing.json.JSON

object Database extends Application {
	val jsonStr = """
	[
		{
			"MISSING_PORTLETS": {
				"context": {},
				"tables": {
					"logs.VapLog": [ "~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/2010[_\d]+\.txt" ],
					"logs.VapPerfLogs": [ "~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/perf_2010[_\d]+\.txt" ],
					"logs.WlsAccessLog": [ "~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/access\.log\d*" ]
				}
			}
		}
	]
	"""

	val json = JSON.parseFull(jsonStr)
	println(json)
}

/*
MISSING_PORTLETS:
	VapLog: ~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/2010[_\d]+\.txt
	VapPerfLogs: ~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/perf_2010[_\d]+\.txt
	WlsAccessLog: ~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/access\.log\d*

{
	Databases: {
		name: "MISSING_PORTLETS",
		tables: {
			VapLog: [ ~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/2010[_\d]+\.txt ]
			VapPerfLogs: [ ~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/perf_2010[_\d]+\.txt ]
			WlsAccessLog: [ ~/Development/Query4Log2/q4l2-hp/src/test/resources/missing_portlets/${serverName}/access\.log\d* ]
		}
	}
}
 */