var PanelView = Backbone.View.extend({

	el: $('#side-panel'),

	defaults: {},

	flowPLotOptions: {
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

	flowPlotData: [
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
		'click #panel-toggle': 'panelToggled',
		'click #close': 'panelToggled'
	},

	activityGridColumns: [{
		cell: 'string',
		name: 'timestamp',
		label: 'Time',
		editable: false,
	},{
		cell: 'string',
		name: 'value',
		label: 'Event',
		editable: false,
	}],

	activityUpdateFrequency: 1000, // update activity every 5 seconds

	initialize: function(){
		// this.chartOptions = bootstrap.chartOptions;
		// this.chartDataFormats = bootstrap.chartDataFormats;
		this.activitiesCollection = new ActivitiesCollection();

		// Make our charts
		this.flowPlot = $.plot(this.$('#flow-chart'), this.flowPlotData, this.flowPlotOptions);

		// make our intersection backgrid
		this.activitiesTable = new Backgrid.Grid({
			columns: this.activityGridColumns,
			collection: this.activitiesCollection
		});

		this.render();
		this.$('#activities-table-container').append(this.activitiesTable.el);
	},

	render: function(){
		this.activitiesTable.render();
	},

	expand: function(){
		this.$el.addClass('expanded');
		this.$('#panel-container').addClass('expanded')
		this.$('#panel-toggle').css('visibility', 'hidden');
		this.$('#close').css('visibility', 'visible');
		this.isExpanded = true;
	},

	collapse: function(){
		// remove all the styling and hide
		this.$('#panel-container').removeClass('expanded');
		this.$el.removeClass('expanded');
		this.$('#panel-toggle').css('visibility', 'visible');
		this.$('#close').css('visibility', 'hidden');
		this.isExpanded = false;

		// stop the pulling
		this.stopInterval();
	},

	panelToggled: function(){
		switch(this.$el.hasClass('expanded')){
			case(true):
				this.collapse();
				break;

			case(false):
				this.expand();
				break;

			default:
				break;
		}
	},

	// populate the intersection name and info
	showIntersectionDetails: function(intersectionModel){

		// set static values
		this.$('#intersection-title').text( intersectionModel.get('name') );
		this.$('#stat-id').html(intersectionModel.get('int_id'));
		this.$('#stat-type').html(intersectionModel.get('type'));

		// set what we do know before live updates kick in
		this.$('#stat-status').html(intersectionModel.get('status'));
		this.$('#stat-currentState').html(intersectionModel.get('currentState'));
		this.$('#stat-nextState').html(intersectionModel.get('nextState'));
		this.$('#stat-nextStateTime').html(intersectionModel.get('nextStateTime'));

		// For anything that requires real-time data, we use a set interval!
		this.startInterval();
	},

	startInterval: function(){
		this.interval = setInterval(function(pv){
			// do your data pull on the intersection
			$.ajax({
				type: "POST",
				datatype: "JSON",
				contentType: "application/json",
				url: "http://localhost:5000/request_dashboard",
				data: JSON.stringify({int_id: 11}),
				async: false
			}).then( function(d){
				// console.log('dashboard data');
				// console.log(d);
				// reset the activity collection with the new list of events.

				// update general status things
				var general = d[0]['general'][0];
				pv.$('#stat-status').html(general['status']);
				pv.$('#stat-currentState').html(general['behaviour']);
				pv.$('#stat-nextState').html(general['plan']);

				//handle time nicely
				// var timeUntilNextState = new Date(general['bhvr_time']*1000 - Date.now()).format('i:s'); //assuming it's in the future
				// pv.$('#stat-nextStateTime').html(timeUntilNextState);

				// change the icon
				pv.$('#dash-state').attr('src', 'assets/' + general['behaviour'].toLowerCase() + '.png');

				// update activity
				pv.activitiesCollection.reset(d[1]['events']);

				// update graphs
				// Flow plot
	        	// for each direction
	        	var flows = d[2]['flows'];
	        	for(var dir = 0; dir < 4; dir++){
	        		var flotSeries = new Array();

	        		//for each point in the array
	        		for(var index = 0; index < Math.min(flows[dir].flow[0].length, flows[dir].flow[1].length); index++ ){
	        			flotSeries.push([ flows[dir].flow[1][index] , flows[dir].flow[0][index] ]); // flip x and y.
	        		}
	        		// put the new data series in the flotData obj
	        		pv.flowPlotData[dir].data = flotSeries;
	        	}
	        	// set the data.
	        	pv.flowPlot.setData(pv.flowPlotData);
	        	pv.flowPlot.setupGrid();
		        pv.flowPlot.draw();


				// tell s
				s.localUpdate();
				s.restConnected();
			})

			//data pull on activity for intersection
			$.ajax({
				type: "POST",
				datatype: "JSON",
				contentType: "application/json",
				url: "http://localhost:5000/request_int_events",
				data: JSON.stringify({int_id: 11}),
				async: false
			}).then( function(d){
				console.log('activity data');
				console.log(d);
				// reset the activity collection with the new list of events.
				pv.activitiesCollection.reset(d);

				// tell s
				s.localUpdate();
				s.restConnected();
			})

			//update anyhting non-static.

		}, this.activityUpdateFrequency, this)
	},

	stopInterval: function(){
		clearInterval(this.interval);
	}
});