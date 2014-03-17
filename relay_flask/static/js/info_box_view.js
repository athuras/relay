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
				url: "http://localhost:5000/request_dashboard",
				data: JSON.stringify({int_id: 11 /*this.currentModel.get('int_id')*/, dt: 1, duration: 60}),
				async: false
			}).then( function(d){
				//what does it look like?
				// console.log(d)
				d = JSON.parse('[{"status": "FAILURE", "behaviour": "NST", "plan": "EWT", "status_time": 1394925246, "bhvr_time": 1394925077, "plan_time": 1394924738}, {"events": [{"timestamp": 1, "int_id": 11, "value": "Light outage"}, {"timestamp": 24, "int_id": 11, "value": "Accident causing major delays."}]}, {"r_out": [[[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 4], [1395015626.0, 5], [1395015625.0, 10], [1395015623.0, 22], [1395015622.0, 25], [1395015621.0, 33], [1395015620.0, 41], [1395015619.0, 30], [1395015618.0, 25], [1395015617.0, 19], [1395015616.0, 11], [1395015615.0, 8], [1395015614.0, 9], [1395015612.0, 4], [1395015611.0, 2]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 5], [1395015625.0, 6], [1395015624.0, 12], [1395015622.0, 29], [1395015621.0, 19], [1395015620.0, 29], [1395015619.0, 41], [1395015618.0, 30], [1395015617.0, 17], [1395015616.0, 23], [1395015615.0, 9], [1395015614.0, 8], [1395015613.0, 7], [1395015612.0, 8], [1395015610.0, 2], [1395015609.0, 1]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 2], [1395015625.0, 5], [1395015624.0, 7], [1395015622.0, 17], [1395015621.0, 24], [1395015620.0, 21], [1395015619.0, 24], [1395015618.0, 35], [1395015617.0, 24], [1395015616.0, 17], [1395015615.0, 19], [1395015614.0, 15], [1395015613.0, 14], [1395015612.0, 3], [1395015611.0, 7], [1395015609.0, 5], [1395015608.0, 5], [1395015607.0, 1], [1395015606.0, 1]], [[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 3], [1395015626.0, 6], [1395015625.0, 10], [1395015624.0, 20], [1395015623.0, 26], [1395015622.0, 32], [1395015620.0, 35], [1395015619.0, 36], [1395015618.0, 28], [1395015617.0, 16], [1395015616.0, 13], [1395015615.0, 7], [1395015614.0, 8], [1395015613.0, 5], [1395015612.0, 3]]], "in": [[[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 4], [1395015626.0, 5], [1395015625.0, 10], [1395015623.0, 22], [1395015622.0, 25], [1395015621.0, 33], [1395015620.0, 41], [1395015619.0, 30], [1395015618.0, 25], [1395015617.0, 19], [1395015616.0, 11], [1395015615.0, 8], [1395015614.0, 9], [1395015612.0, 4], [1395015611.0, 2]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 5], [1395015625.0, 6], [1395015624.0, 12], [1395015622.0, 29], [1395015621.0, 19], [1395015620.0, 29], [1395015619.0, 41], [1395015618.0, 30], [1395015617.0, 17], [1395015616.0, 23], [1395015615.0, 9], [1395015614.0, 8], [1395015613.0, 7], [1395015612.0, 8], [1395015610.0, 2], [1395015609.0, 1]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 2], [1395015625.0, 5], [1395015624.0, 7], [1395015622.0, 17], [1395015621.0, 24], [1395015620.0, 21], [1395015619.0, 24], [1395015618.0, 35], [1395015617.0, 24], [1395015616.0, 17], [1395015615.0, 19], [1395015614.0, 15], [1395015613.0, 14], [1395015612.0, 3], [1395015611.0, 7], [1395015609.0, 5], [1395015608.0, 5], [1395015607.0, 1], [1395015606.0, 1]], [[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 3], [1395015626.0, 6], [1395015625.0, 10], [1395015624.0, 20], [1395015623.0, 26], [1395015622.0, 32], [1395015620.0, 35], [1395015619.0, 36], [1395015618.0, 28], [1395015617.0, 16], [1395015616.0, 13], [1395015615.0, 7], [1395015614.0, 8], [1395015613.0, 5], [1395015612.0, 3]]], "out": [[[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 4], [1395015626.0, 5], [1395015625.0, 10], [1395015623.0, 22], [1395015622.0, 25], [1395015621.0, 33], [1395015620.0, 41], [1395015619.0, 30], [1395015618.0, 25], [1395015617.0, 19], [1395015616.0, 11], [1395015615.0, 8], [1395015614.0, 9], [1395015612.0, 4], [1395015611.0, 2]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 5], [1395015625.0, 6], [1395015624.0, 12], [1395015622.0, 29], [1395015621.0, 19], [1395015620.0, 29], [1395015619.0, 41], [1395015618.0, 30], [1395015617.0, 17], [1395015616.0, 23], [1395015615.0, 9], [1395015614.0, 8], [1395015613.0, 7], [1395015612.0, 8], [1395015610.0, 2], [1395015609.0, 1]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 2], [1395015625.0, 5], [1395015624.0, 7], [1395015622.0, 17], [1395015621.0, 24], [1395015620.0, 21], [1395015619.0, 24], [1395015618.0, 35], [1395015617.0, 24], [1395015616.0, 17], [1395015615.0, 19], [1395015614.0, 15], [1395015613.0, 14], [1395015612.0, 3], [1395015611.0, 7], [1395015609.0, 5], [1395015608.0, 5], [1395015607.0, 1], [1395015606.0, 1]], [[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 3], [1395015626.0, 6], [1395015625.0, 10], [1395015624.0, 20], [1395015623.0, 26], [1395015622.0, 32], [1395015620.0, 35], [1395015619.0, 36], [1395015618.0, 28], [1395015617.0, 16], [1395015616.0, 13], [1395015615.0, 7], [1395015614.0, 8], [1395015613.0, 5], [1395015612.0, 3]]]}]');

				// update general status things
				var general = d[0];
				// var general = d[0]['general'][0];
				$(ibv.boxText).find('#info-box-status').get(0).innerHTML = general['status'];
				$(ibv.boxText).find('#info-box-currentState').get(0).innerHTML = general['behaviour'];
				$(ibv.boxText).find('#info-box-nextState').get(0).innerHTML = general['plan'];

				//handle time nicely
				// var timeUntilNextState = new Date(general['bhvr_time']*1000 - Date.now()).format('i:s'); //assuming it's in the future
				// $(ibv.boxText).find('#info-box-nextStateTime').get(0).innerHTML = timeUntilNextState;

				// change the icon
				$($(ibv.boxText).find('#icon-state').get(0)).attr('src', 'assets/' + general['behaviour'].toLowerCase() + '.png');

				// update graphs
				// Flow plot
	        	// for each direction
	        	// var flows = d[2]['flows'];
	        	// for(var dir = 0; dir < 4; dir++){
	        	// 	var flotSeries = new Array();

	        	// 	//for each point in the array
	        	// 	for(var index = 0; index < Math.min(flows[dir].flow[0].length, flows[dir].flow[1].length); index++ ){
	        	// 		flotSeries.push([ flows[dir].flow[1][index] , flows[dir].flow[0][index] ]); // flip x and y.
	        	// 	}
	        	// 	// put the new data series in the flotData obj
	        	// 	ibv.flotData[dir].data = flotSeries;
	        	// }
	        	// add flow data
	        	inFlows = d[2]['in'];
	        	for(var dir = 0; dir < 4; dir++){
	        		ibv.flotData[dir].data = inFlows[dir];
	        	}

				// set the data.
				ibv.flotPlot.setData(ibv.flotData);
				ibv.flotPlot.setupGrid();
				ibv.flotPlot.draw();

				// update s
				s.localUpdate();
				s.restConnected();

			});;
		}, this.updateFrequency, this);
	},

	stopInterval: function(){
 		clearInterval(this.interval);
	}

});