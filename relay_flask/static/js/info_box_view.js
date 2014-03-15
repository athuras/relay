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

		// make the graph
		var graph = new Rickshaw.Graph({
			element: $(this.boxText).find('#graph').get(0),
			width: 250,
			height: 100,
			renderer: 'multi',

		    series: [
		    {
				color: 'white',
				// data should come from model
			    data: [
			    { x: 1394850520, y: 3 },
			    { x: 1394850535, y: 5 },
			    { x: 1394850550, y: 3 },
			    { x: 1394850565, y: 5 },
			    ],
			    renderer: 'bar'
			},{
			    color: 'steelblue',
			    // data should come from model
			    data: [
			    { x: 1394850520, y: 2 },
			    { x: 1394850535, y: 4 },
			    { x: 1394850550, y: 2 },
			    { x: 1394850565, y: 4 },
			    { x: 1394850580, y: 2 },
			    { x: 1394850595, y: 4 },
			    { x: 1394850610, y: 2 },
			    { x: 1394850625, y: 4 },
			    ],
			    renderer: 'line'
			}],
		});

		// add axes
		var xAxes = new Rickshaw.Graph.Axis.Time( { graph: graph } );
		var yAxes = new Rickshaw.Graph.Axis.Y( {
			graph: graph,
			orientation: 'left',
			tickFormat: Rickshaw.Fixtures.Number.formatKMBT,
			element: $(this.boxText).find('#legend').get(0)
		});

		// var graph = new Rickshaw.Graph( {
		//         element: $(this.boxText).find('#graph').get(0),
		//         width: 540,
		//         height: 240,
		//         series: [ {
		//                 data: [ { x: -1893456000, y: 92228531 }, { x: -1577923200, y: 106021568 }, { x: -1262304000, y: 123202660 }, { x: -946771200, y: 132165129 }, { x: -631152000, y: 151325798 }, { x: -315619200, y: 179323175 }, { x: 0, y: 203211926 }, { x: 315532800, y: 226545805 }, { x: 631152000, y: 248709873 }, { x: 946684800, y: 281421906 }, { x: 1262304000, y: 308745538 } ], 
		//                 color: 'steelblue'
		//         } ]
		// } );

		// var x_axis = new Rickshaw.Graph.Axis.Time( { graph: graph } );

		// var y_axis = new Rickshaw.Graph.Axis.Y( {
		//         graph: graph,
		//         orientation: 'left',
		//         tickFormat: Rickshaw.Fixtures.Number.formatKMBT,
		//         element: $(this.boxText).find('#graph').get(0),
		// } );

		// graph.render();

		// render graph
		graph.render();

		// add an event listener to check for 'show more' click
		google.maps.event.addDomListener(this.boxText, 'click', $.proxy(function(e){
			// console.log(e);
			this.infoBoxClicked(e);
		}, this));
	},

	close: function(){
		this.infoBox.close();
		this.isOpen = false;
	},

	infoBoxClicked: function(e){
		console.log(e);
		console.log(e.toElement);
		if($(this.boxText).find('#more-info-link').get(0) === e.toElement){
			this.mapPageView.showMore(this.currentModel);
		}
	}

});