var InfoBoxView = Backbone.View.extend({
	el: $('#info-box'),

	defaults: {
		'currentModel': null,
		'currentMarker': null,
		'showMoreDom': null,
	},

	flotOptions: {
		xaxis: {
			// position: "bottom",
			// reserveSpace: -10,
			min: 0,
			max: 10
		},
		yaxis: {
			// position: "left",
			reserveSpace: -10,
			min: 0,
			max: 50
		},
		legend: {
			backgroundColor: 'rgba(0, 0, 0, 0)',
			backgroundOpacity: 0,
			labelBoxBorderColor: 'rgba(0, 0, 0, 0)',
			labelFormatter: function(label, series) {
				return '<div style="color: #cecece; position: relative; top: -4px;">' + label + '</div>';
			},
			margin: [-3, 32]
		},
		grid: {
			borderColor: 'rgba(0, 0, 0, 0)',
			color: 'rgba(54, 54, 54, 0.6)'
		}
	},

	flotData: [
	{
		data: null,
		label: 'North',
		color: 'rgba(0, 210, 250, 0.8)'
	},{
		data: null,
		label: 'South',
		color: 'rgba(0, 250, 200, 0.8)'
	},{
		data: null,
		label: 'East',
		color: 'rgba(209, 27, 45, 0.7)'
	},{
		data: null,
		label: 'West',
		color: 'rgba(250, 137, 52, 0.7)'
	}],

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
		this.updateFrequency = 1000;
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

		this.flotPlot = $.plot($(this.boxText).find('#graph').get(0), this.flotData, this.flotOptions);

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
		this.interval = setInterval(function(ibv){
				// run the request for flow data,
				$.ajax({
					type: "POST",
					datatype: "JSON",
					contentType: "application/json",
					url: "http://localhost:5000/request_flows",
					data: JSON.stringify({int_id: 11 /*this.currentModel.get('int_id')*/, dt: 1, duration: 60}),
					async: false
		        }).then( function(d){
		        	//what does it look like?
		        	console.log(d)

		        	// for each direction
		        	for(var dir = 0; dir < 4; dir++){
		        		var flotSeries = new Array();

		        		//for each point in the array
		        		for(var index = 0; index < Math.min(d[dir].flow[0].length, d[dir].flow[1].length); index++ ){
		        			flotSeries.push([ d[dir].flow[1][index] , d[dir].flow[0][index] ]); // flip x and y.
		        		}
		        		// put the new data series in the flotData obj
		        		ibv.flotData[dir].data = flotSeries;
		        	}

		        	// set the data.
		        	ibv.flotPlot.setData(ibv.flotData);
		        	ibv.flotPlot.setupGrid();
		        	ibv.flotPlot.draw();

					// update s
					s.localUpdate();
					s.restConnected()

		        });;

			}, this.updateFrequency, this);
	},

	stopInterval: function(){
 		clearInterval(this.interval);
	}

});