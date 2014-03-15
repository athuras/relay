var InfoBoxView = Backbone.View.extend({
	el: $('#info-box'),

	defaults: {
		'currentModel': null,
		'currentMarker': null,
		'showMoreDom': null,
	},

	events: {
		'click #more-info-link': 'showMoreClicked',
		'click' : 'clicked'
	},

	initialize: function(options){
		// we keep reference of the map for convenience.
		this.map = options.map;
		this.mapPageView = options.mapPageView;

		// info box options use these to style the box!
		this.boxText = $('<div></div>').loadTemplate('#info-box-template', {}).get(0)
		var myOptions = {
			content: this.boxText
			,disableAutoPan: false
			,maxWidth: 0
			,pixelOffset: new google.maps.Size(-10, 25)
			,zIndex: null
			,closeBoxMargin: ""
			,closeBoxURL: ""
			,infoBoxClearance: new google.maps.Size(1, 1)
			,isHidden: false
			,pane: "floatPane"
			,enableEventPropagation: false
		};

		// interval for pulling data
		this.updateFrequency = 100;
		this.interval = setInterval();

		// We create ourselves an infobox
		this.infoBox = new InfoBox(myOptions);

		this.render();
	},

	render: function(){
	},

	setIntersection: function(marker, model){
		this.currentMarker = marker;
		this.currentModel = model;

		this.infoBox.close(); // close it if it's already on the map.
		// set the content of the info box to that of the new intersection
		this.boxText = $('<div></div>').loadTemplate('#info-box-template', model.attributes).get(0);
		this.showMoreDom = $('#more-info-link').get(0);
		this.infoBox.setContent(this.boxText);

		// open it on the map
		this.infoBox.open(this.map, marker);
		this.isOpen = true;

		this.flowGraph = new Rickshaw.Graph( {
			element: /*$(this.boxText).find('#graph').get(0),*/ document.getElementById("graph"),
			renderer: 'multi',
			maxDataPoints: 5,
			height: 150,
			width: 270,
			series: [
				{
					name: '1',
					data: [ { x: 1394857840, y: 120 }, 
					{ x: 1394857841, y: 890 }, 
					{ x: 1394857842, y: 38 }, 
					{ x: 1394857843, y: 70 }, 
					{ x: 1394857844, y: 32 },
					{ x: 1394857845, y: 120 }, 
					{ x: 1394857846, y: 890 }, 
					{ x: 1394857847, y: 38 }, 
					{ x: 1394857848, y: 70 }, 
					{ x: 1394857849, y: 32 } ],
					color: "#c05020",
					renderer: 'line'
				}, {
					name: '2',
					data: [ { x: 1394857840, y: 80 }, 
					{ x: 1394857841, y: 200 }, 
					{ x: 1394857842, y: 100 }, 
					{ x: 1394857843, y: 520 }, 
					{ x: 1394857844, y: 133 },
					{ x: 1394857845, y: 80 }, 
					{ x: 1394857846, y: 200 }, 
					{ x: 1394857847, y: 100 }, 
					{ x: 1394857848, y: 520 }, 
					{ x: 1394857849, y: 133 } ],
					color: "#30c020",
					renderer: 'line'
				},{
					name: '3',
					data: [ { x: 1394857840, y: 120 }, 
					{ x: 1394857841, y: 890 }, 
					{ x: 1394857842, y: 38 }, 
					{ x: 1394857843, y: 70 }, 
					{ x: 1394857844, y: 32 },
					{ x: 1394857845, y: 120 }, 
					{ x: 1394857846, y: 890 }, 
					{ x: 1394857847, y: 38 }, 
					{ x: 1394857848, y: 70 }, 
					{ x: 1394857849, y: 32 } ],
					color: "#fff",
					renderer: 'line'
				}, {
					name: '4',
					data: [ { x: 1394857840, y: 80 }, 
					{ x: 1394857841, y: 200 }, 
					{ x: 1394857842, y: 100 }, 
					{ x: 1394857843, y: 520 }, 
					{ x: 1394857844, y: 133 },
					{ x: 1394857845, y: 80 }, 
					{ x: 1394857846, y: 200 }, 
					{ x: 1394857847, y: 100 }, 
					{ x: 1394857848, y: 520 }, 
					{ x: 1394857849, y: 133 } ],
					color: "#999",
					renderer: 'line'
				}
			]
		} );

		var y_ticks = new Rickshaw.Graph.Axis.Y( {
			graph: this.flowGraph,
			orientation: 'left',
			tickFormat: Rickshaw.Fixtures.Number.formatKMBT,
			element: /*$(this.boxText).find('#yaxis').get(0),*/ document.getElementById('yaxis'),
		} );

		var time = new Rickshaw.Fixtures.Time();
		var seconds = time.unit('second');

		var xAxis = new Rickshaw.Graph.Axis.Time({
		    graph: this.flowGraph,
		    timeUnit: seconds,
		    orientation: 'bottom'
		});

		this.flowGraph.render();

		// add an event listener to check for 'show more' click
		google.maps.event.addDomListener(this.boxText, 'click', $.proxy(function(e){
			// console.log(e);
			this.infoBoxClicked(e);
		}, this));

		// start the update loop
		this.startInterval();
	},

	close: function(){
		this.infoBox.close();
		this.stopInterval();
		this.isOpen = false;
	},

	// listens for clicks and if they're on the link, we send the itersection to the panel.
	infoBoxClicked: function(e){
		console.log(e);
		console.log(e.toElement);
		if($(this.boxText).find('#chunk-arrow').get(0) === e.toElement){
			this.mapPageView.showMore(this.currentModel);
		}
	},

	startInterval: function(){
		this.interval = setInterval(function(e){
				// run the request for flow data,
				$.ajax({
					type: "POST",
					datatype: "JSON",
					contentType: "application/json",
					url: "http://localhost:5000/request_flow",
					data: JSON.stringify({int_id: 11 /*this.currentModel.get('int_id')*/}),
					async: false
		        }).then( function(/*d*/){
		        	//When the data gets back,
		        	// d = {
		        	// 	flows:[
		        	// 		[
		        	// 			[,2,3,4,5],
		        	// 			[0,1,2,3,4]
	        		// 		],[
			        // 			[6,7,8,9,0],
			        // 			[0,1,2,3,4]
		        	// 		],[
			        // 			[5,4,3,2,1],
			        // 			[0,1,2,3,4]
		        	// 		],[
			        // 			[0,9,8,7,6]
			        // 			[0,1,2,3,4]
		        	// 		]
		        	// 	],
		        	// 	timestamp: 1394909778
		        	// };
		        	// // edit data in d for timestamps
		        	// for(var dir = 0; dir < 4; dir++){
		        	// 	for(var entry = 0; entry < flows[0][0].length; dir++){
		        	// 		d.flows[dir][1 /* for timestamp row*/ ][entry] += d.timestamp;
		        	// 	}
		        	// 	e.flowGraph.series[dir].data = flow[dir];
		        	// }
		        });;

		        var d = {
	        		flows:[
	        			[
	        				[1,2,3,4,5],
	        				[0,1,2,3,4]
        				],[
		        			[6,7,8,9,0],
		        			[0,1,2,3,4]
	        			],[
		        			[5,4,3,2,1],
		        			[0,1,2,3,4]
	        			],[
		        			[0,9,8,7,6],
		        			[0,1,2,3,4]
	        			]
	        		],
	        		timestamp: 1394909778
	        	};
	        	// edit data in d for timestamps
	        	for(var dir = 0; dir < 4; dir++){
	        		for(var entry = 0; entry < d.flows[0][0].length; entry++){
	        			d.flows[dir][1 /* for timestamp row*/ ][entry] += d.timestamp;
	        		}
	        		e.flowGraph.series[dir].addData(d.flows[dir]);
	        	}

				// redraw the graph
		        e.flowGraph.render();
			}, this.updateFrequency, this);
	},

	stopInterval: function(){
 		clearInterval(this.interval);
	}

});