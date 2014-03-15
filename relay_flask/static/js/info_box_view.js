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
			width: 300,
			height: 150,
			renderer: 'multi',

		    series: [
		    {
		    	name: 'historical',
				color: 'rgba(15, 150, 207, 0.4)',
				// data should come from model
			    data: [
			    { x: 0, y: 3 },
			    { x: 1, y: 4 },
			    { x: 2, y: 5 },
			    { x: 3, y: 2 },
			    { x: 4, y: 1 },
			    { x: 5, y: 5 },
			    { x: 6, y: 5 },
			    { x: 7, y: 4 },
			    ],
			    renderer: 'bar'
			},{
				name: 'predicted bars',
			    color: 'rgba(15, 150, 207, 0.2)',
			    // data should come from model
			    data: [
			    { x: 8, y: 3 },
			    { x: 9, y: 4 },
			    { x: 10, y: 5 },
			    { x: 11, y: 2 },
			    { x: 12, y: 1 },
			    { x: 13, y: 5 },
			    { x: 14, y: 5 },
			    { x: 15, y: 4 },
			    ],
			    renderer: 'bar'
			},{
				name: 'predicted line',
			    color: 'rgba(60, 180, 212, 0.9)',
			    // data should come from model
			    data: [
			    { x: 8, y: 3 },
			    { x: 9, y: 4 },
			    { x: 10, y: 5 },
			    { x: 11, y: 2 },
			    { x: 12, y: 1 },
			    { x: 13, y: 5 },
			    { x: 14, y: 5 },
			    { x: 15, y: 4 },
			    ],
			    renderer: 'line'
			}]
		});

		// add axes
		var xAxis = new Rickshaw.Graph.Axis.Time({
			graph: graph
		});

		var yAxis = new Rickshaw.Graph.Axis.Y( {
			graph: graph,
			orientation: 'left',
			// tickFormat: Rickshaw.Fixtures.Number.formatKMBT,
			element: document.getElementById('yaxis'),
		});

		// render graph

		graph.render();
				xAxis.render();
		yAxis.render();

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

	// listens for clicks and if they're on the link, we send the itersection to the panel.
	infoBoxClicked: function(e){
		console.log(e);
		console.log(e.toElement);
		if($(this.boxText).find('#chunk-arrow').get(0) === e.toElement){
			this.mapPageView.showMore(this.currentModel);
		}
	}

});