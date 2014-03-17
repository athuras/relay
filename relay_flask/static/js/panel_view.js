var PanelView = Backbone.View.extend({

	el: $('#side-panel'),

	defaults: {},

	flowPlotOptions: {
		xaxis: {
			mode: "time",
			timeformat: "%I:%M:%S",
			minTickSize: [1, "second"]
		},
		yaxis: {
			min: 0,
			max: 12
		},
		legend: {
			backgroundColor: 'rgba(0, 0, 0, 0)',
			backgroundOpacity: 0,
			labelBoxBorderColor: 'rgba(0, 0, 0, 0)',
			labelFormatter: function(label, series) {
				return '<div style="font-size: 0.7em; padding: 0 6px; color: #cecece; position: relative; top: -4px;">' + label + '</div>';
			},
			margin: [-120, 0],
			noColumns: 2
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
	},{
		data: null,
		label: 'North Predict',
		color: 'rgba(0, 210, 250, 0.6)'
	},{
		data: null,
		label: 'South Predict',
		color: 'rgba(0, 250, 200, 0.6)'
	},{
		data: null,
		label: 'East Predict',
		color: 'rgba(209, 27, 45, 0.5)'
	},{
		data: null,
		label: 'West Predict',
		color: 'rgba(250, 137, 52, 0.5)'
	}],

	perfPlotOptions: {
		xaxis: {
			mode: "time",
			timeformat: "%I:%M:%S"
		},
		yaxis: {
			min: 0,
			max: 2
		},
		legend: {
			backgroundColor: 'rgba(0, 0, 0, 0)',
			backgroundOpacity: 0,
			labelBoxBorderColor: 'rgba(0, 0, 0, 0)',
			labelFormatter: function(label, series) {
				return '<div style="font-size: 0.7em; padding: 0 6px; color: #cecece; position: relative; top: -4px;">' + label + '</div>';
			},
			margin: [-120, 0],
			noColumns: 2
		},
		grid: {
			borderColor: 'rgba(0, 0, 0, 0)',
			color: 'rgba(54, 54, 54, 0.6)'
		}
	},

	perfPlotData: [
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
	},{
		data: null,
		label: 'North Predict',
		color: 'rgba(0, 210, 250, 0.6)'
	},{
		data: null,
		label: 'South Predict',
		color: 'rgba(0, 250, 200, 0.6)'
	},{
		data: null,
		label: 'East Predict',
		color: 'rgba(209, 27, 45, 0.5)'
	},{
		data: null,
		label: 'West Predict',
		color: 'rgba(250, 137, 52, 0.5)'
	}],

	events: {
		'click #panel-toggle': 'panelToggled',
		'click #close': 'panelToggled'
	},

	activityGridColumns: [{
		cell: Backgrid.DateCell,
		name: 'timestamp',
		label: 'Date',
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
		this.currentIntersectionModel = null;

		// Make our charts
		this.flowPlot = $.plot(this.$('#flow-chart'), this.flowPlotData, this.flowPlotOptions);
		this.perfPlot = $.plot(this.$('#perf-chart'), this.perfPlotData, this.perfPlotOptions);

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
		this.currentIntersectionModel = intersectionModel;

		// set static values
		this.$('#intersection-title').text( intersectionModel.get('name') );
		this.$('#stat-id').html(intersectionModel.get('int_id'));
		this.$('#stat-type').html(intersectionModel.get('type'));

		// set what we do know before live updates kick in
		this.$('#stat-status').html(intersectionModel.get('status'));
		this.$('#stat-currentState').html(intersectionModel.get('behaviour'));
		this.$('#stat-nextState').html(intersectionModel.get('plan'));

		//handle time nicely
		var stateDuration = new Date((intersectionModel.get('plan_time')-intersectionModel.get('bhvr_time')) ).format('i:s');
		this.$('#stat-duration').html(stateDuration);
		var timeUntilNextState = new Date(intersectionModel.get('plan_time') - Date.now()).format('i:s'); //assuming it's in the future
		this.$('#stat-nextStateTime').html(timeUntilNextState);

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
				data: JSON.stringify({int_id: pv.currentIntersectionModel.get('int_id'), duration: 60}),
				async: false
			}).then( function(d){
				// console.log('dashboard data');
				// console.log(d);
				// reset the activity collection with the new list of events.

				// ***** To use real data, comment out these lines
				
				//d = JSON.parse('[{"status": "FAILURE", "behaviour": "NST", "plan": "EWT", "status_time": 1394925246, "bhvr_time": 1394925077, "plan_time": 1394924738}, {"events": [{"timestamp": 1, "int_id": 11, "value": "Light outage"}, {"timestamp": 24, "int_id": 11, "value": "Accident causing major delays."}]}, {"r_out": [[[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 4], [1395015626.0, 5], [1395015625.0, 10], [1395015623.0, 22], [1395015622.0, 25], [1395015621.0, 33], [1395015620.0, 41], [1395015619.0, 30], [1395015618.0, 25], [1395015617.0, 19], [1395015616.0, 11], [1395015615.0, 8], [1395015614.0, 9], [1395015612.0, 4], [1395015611.0, 2]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 5], [1395015625.0, 6], [1395015624.0, 12], [1395015622.0, 29], [1395015621.0, 19], [1395015620.0, 29], [1395015619.0, 41], [1395015618.0, 30], [1395015617.0, 17], [1395015616.0, 23], [1395015615.0, 9], [1395015614.0, 8], [1395015613.0, 7], [1395015612.0, 8], [1395015610.0, 2], [1395015609.0, 1]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 2], [1395015625.0, 5], [1395015624.0, 7], [1395015622.0, 17], [1395015621.0, 24], [1395015620.0, 21], [1395015619.0, 24], [1395015618.0, 35], [1395015617.0, 24], [1395015616.0, 17], [1395015615.0, 19], [1395015614.0, 15], [1395015613.0, 14], [1395015612.0, 3], [1395015611.0, 7], [1395015609.0, 5], [1395015608.0, 5], [1395015607.0, 1], [1395015606.0, 1]], [[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 3], [1395015626.0, 6], [1395015625.0, 10], [1395015624.0, 20], [1395015623.0, 26], [1395015622.0, 32], [1395015620.0, 35], [1395015619.0, 36], [1395015618.0, 28], [1395015617.0, 16], [1395015616.0, 13], [1395015615.0, 7], [1395015614.0, 8], [1395015613.0, 5], [1395015612.0, 3]]], "in": [[[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 4], [1395015626.0, 5], [1395015625.0, 10], [1395015623.0, 22], [1395015622.0, 25], [1395015621.0, 33], [1395015620.0, 41], [1395015619.0, 30], [1395015618.0, 25], [1395015617.0, 19], [1395015616.0, 11], [1395015615.0, 8], [1395015614.0, 9], [1395015612.0, 4], [1395015611.0, 2]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 5], [1395015625.0, 6], [1395015624.0, 12], [1395015622.0, 29], [1395015621.0, 19], [1395015620.0, 29], [1395015619.0, 41], [1395015618.0, 30], [1395015617.0, 17], [1395015616.0, 23], [1395015615.0, 9], [1395015614.0, 8], [1395015613.0, 7], [1395015612.0, 8], [1395015610.0, 2], [1395015609.0, 1]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 2], [1395015625.0, 5], [1395015624.0, 7], [1395015622.0, 17], [1395015621.0, 24], [1395015620.0, 21], [1395015619.0, 24], [1395015618.0, 35], [1395015617.0, 24], [1395015616.0, 17], [1395015615.0, 19], [1395015614.0, 15], [1395015613.0, 14], [1395015612.0, 3], [1395015611.0, 7], [1395015609.0, 5], [1395015608.0, 5], [1395015607.0, 1], [1395015606.0, 1]], [[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 3], [1395015626.0, 6], [1395015625.0, 10], [1395015624.0, 20], [1395015623.0, 26], [1395015622.0, 32], [1395015620.0, 35], [1395015619.0, 36], [1395015618.0, 28], [1395015617.0, 16], [1395015616.0, 13], [1395015615.0, 7], [1395015614.0, 8], [1395015613.0, 5], [1395015612.0, 3]]], "out": [[[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 4], [1395015626.0, 5], [1395015625.0, 10], [1395015623.0, 22], [1395015622.0, 25], [1395015621.0, 33], [1395015620.0, 41], [1395015619.0, 30], [1395015618.0, 25], [1395015617.0, 19], [1395015616.0, 11], [1395015615.0, 8], [1395015614.0, 9], [1395015612.0, 4], [1395015611.0, 2]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 5], [1395015625.0, 6], [1395015624.0, 12], [1395015622.0, 29], [1395015621.0, 19], [1395015620.0, 29], [1395015619.0, 41], [1395015618.0, 30], [1395015617.0, 17], [1395015616.0, 23], [1395015615.0, 9], [1395015614.0, 8], [1395015613.0, 7], [1395015612.0, 8], [1395015610.0, 2], [1395015609.0, 1]], [[1395015629.0, 1], [1395015628.0, 1], [1395015627.0, 2], [1395015626.0, 2], [1395015625.0, 5], [1395015624.0, 7], [1395015622.0, 17], [1395015621.0, 24], [1395015620.0, 21], [1395015619.0, 24], [1395015618.0, 35], [1395015617.0, 24], [1395015616.0, 17], [1395015615.0, 19], [1395015614.0, 15], [1395015613.0, 14], [1395015612.0, 3], [1395015611.0, 7], [1395015609.0, 5], [1395015608.0, 5], [1395015607.0, 1], [1395015606.0, 1]], [[1395015629.0, 2], [1395015628.0, 0], [1395015627.0, 3], [1395015626.0, 6], [1395015625.0, 10], [1395015624.0, 20], [1395015623.0, 26], [1395015622.0, 32], [1395015620.0, 35], [1395015619.0, 36], [1395015618.0, 28], [1395015617.0, 16], [1395015616.0, 13], [1395015615.0, 7], [1395015614.0, 8], [1395015613.0, 5], [1395015612.0, 3]]]}]');
				// d = JSON.parse('[{"behaviour": "NST", "bhvr_time": 1394925077, "plan": "EWT", "plan_time": 1394924738, "status": "FAILURE", "status_time": 1394925246}, {"events": [{"int_id": 11, "timestamp": 1, "value": "Light outage"}, {"int_id": 11, "timestamp": 24, "value": "Accident causing major delays."}]}, {"in": [[[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 13], [1395029355.0, 13], [1395029354.0, 20], [1395029353.0, 19], [1395029351.0, 32], [1395029350.0, 26], [1395029349.0, 25], [1395029348.0, 33], [1395029347.0, 27], [1395029346.0, 13], [1395029345.0, 9], [1395029344.0, 9], [1395029343.0, 1], [1395029342.0, 2], [1395029341.0, 1], [1395029340.0, 1]], [[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 11], [1395029355.0, 15], [1395029354.0, 19], [1395029353.0, 17], [1395029352.0, 27], [1395029350.0, 23], [1395029349.0, 29], [1395029348.0, 23], [1395029347.0, 23], [1395029346.0, 25], [1395029345.0, 11], [1395029344.0, 12], [1395029343.0, 3], [1395029342.0, 2], [1395029341.0, 2], [1395029340.0, 1], [1395029339.0, 0], [1395029338.0, 1]], [[1395029358.0, 3], [1395029357.0, 5], [1395029356.0, 9], [1395029355.0, 14], [1395029354.0, 16], [1395029353.0, 18], [1395029351.0, 28], [1395029350.0, 16], [1395029349.0, 20], [1395029348.0, 27], [1395029347.0, 22], [1395029346.0, 16], [1395029345.0, 18], [1395029344.0, 16], [1395029343.0, 10], [1395029342.0, 4], [1395029341.0, 2], [1395029340.0, 4], [1395029338.0, 0], [1395029337.0, 0], [1395029336.0, 1], [1395029335.0, 0], [1395029334.0, 1]], [[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 13], [1395029355.0, 17], [1395029354.0, 19], [1395029352.0, 21], [1395029351.0, 33], [1395029350.0, 26], [1395029349.0, 26], [1395029348.0, 32], [1395029347.0, 26], [1395029346.0, 15], [1395029345.0, 8], [1395029344.0, 4], [1395029342.0, 2], [1395029341.0, 0], [1395029340.0, 2]]], "out": [[[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 13], [1395029355.0, 13], [1395029354.0, 20], [1395029353.0, 19], [1395029351.0, 32], [1395029350.0, 26], [1395029349.0, 25], [1395029348.0, 33], [1395029347.0, 27], [1395029346.0, 13], [1395029345.0, 9], [1395029344.0, 9], [1395029343.0, 1], [1395029342.0, 2], [1395029341.0, 1], [1395029340.0, 1]], [[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 11], [1395029355.0, 15], [1395029354.0, 19], [1395029353.0, 17], [1395029352.0, 27], [1395029350.0, 23], [1395029349.0, 29], [1395029348.0, 23], [1395029347.0, 23], [1395029346.0, 25], [1395029345.0, 11], [1395029344.0, 12], [1395029343.0, 3], [1395029342.0, 2], [1395029341.0, 2], [1395029340.0, 1], [1395029339.0, 0], [1395029338.0, 1]], [[1395029358.0, 3], [1395029357.0, 5], [1395029356.0, 9], [1395029355.0, 14], [1395029354.0, 16], [1395029353.0, 18], [1395029351.0, 28], [1395029350.0, 16], [1395029349.0, 20], [1395029348.0, 27], [1395029347.0, 22], [1395029346.0, 16], [1395029345.0, 18], [1395029344.0, 16], [1395029343.0, 10], [1395029342.0, 4], [1395029341.0, 2], [1395029340.0, 4], [1395029338.0, 0], [1395029337.0, 0], [1395029336.0, 1], [1395029335.0, 0], [1395029334.0, 1]], [[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 13], [1395029355.0, 17], [1395029354.0, 19], [1395029352.0, 21], [1395029351.0, 33], [1395029350.0, 26], [1395029349.0, 26], [1395029348.0, 32], [1395029347.0, 26], [1395029346.0, 15], [1395029345.0, 8], [1395029344.0, 4], [1395029342.0, 2], [1395029341.0, 0], [1395029340.0, 2]]], "prediction": [[[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 13], [1395029355.0, 13], [1395029354.0, 20], [1395029353.0, 19], [1395029351.0, 32], [1395029350.0, 26], [1395029349.0, 25], [1395029348.0, 33], [1395029347.0, 27], [1395029346.0, 13], [1395029345.0, 9], [1395029344.0, 9], [1395029343.0, 1], [1395029342.0, 2], [1395029341.0, 1], [1395029340.0, 1]], [[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 11], [1395029355.0, 15], [1395029354.0, 19], [1395029353.0, 17], [1395029352.0, 27], [1395029350.0, 23], [1395029349.0, 29], [1395029348.0, 23], [1395029347.0, 23], [1395029346.0, 25], [1395029345.0, 11], [1395029344.0, 12], [1395029343.0, 3], [1395029342.0, 2], [1395029341.0, 2], [1395029340.0, 1], [1395029339.0, 0], [1395029338.0, 1]], [[1395029358.0, 3], [1395029357.0, 5], [1395029356.0, 9], [1395029355.0, 14], [1395029354.0, 16], [1395029353.0, 18], [1395029351.0, 28], [1395029350.0, 16], [1395029349.0, 20], [1395029348.0, 27], [1395029347.0, 22], [1395029346.0, 16], [1395029345.0, 18], [1395029344.0, 16], [1395029343.0, 10], [1395029342.0, 4], [1395029341.0, 2], [1395029340.0, 4], [1395029338.0, 0], [1395029337.0, 0], [1395029336.0, 1], [1395029335.0, 0], [1395029334.0, 1]], [[1395029358.0, 2], [1395029357.0, 4], [1395029356.0, 13], [1395029355.0, 17], [1395029354.0, 19], [1395029352.0, 21], [1395029351.0, 33], [1395029350.0, 26], [1395029349.0, 26], [1395029348.0, 32], [1395029347.0, 26], [1395029346.0, 15], [1395029345.0, 8], [1395029344.0, 4], [1395029342.0, 2], [1395029341.0, 0], [1395029340.0, 2]]]}, [{"bhvr_mtx": [[0.0, 0.1, 0.6, 0.3], [1.0, 0.0, 0.0, 0.0], [0.7, 0.2, 0.0, 0.1], [0.0, 0.0, 1.0, 0.0]]}, {"plan_times": [1395029396], "plans": ["NST"]}]]');
				// d = JSON.parse('[{"behaviour": "ET", "bhvr_time": 1394925091, "plan": "EWT", "plan_time": 1394924738, "status": "OK", "status_time": 1395034890}, {"events": [{"int_id": 13464373, "timestamp * 1000": 1394931060000, "value": "Scheduled Maintenance"}, {"int_id": 13464373, "timestamp * 1000": 1394571766000, "value": "Light outage"}]}, {"in": [[], [[13990761, 3], [13990763, 1], [13990765, 1], [13990767, 1], [13990769, 1], [13990771, 1], [13990773, 1]], [[13990761, 5]], [[13990761, 2]]], "out": [[[13990761, 0.6000000000000001], [13990763, 0.2], [13990765, 0.2], [13990767, 0.2], [13990769, 0.2], [13990771, 0.2], [13990773, 0.2]], [[13990761, 1.0]], [[13990761, 0.4]], []], "prediction": 0}, [{"bhvr_mtx": [[0.0, 0.0, 0.7, 0.2], [0.2, 0.0, 0.0, 0.0], [0.7, 0.2, 0.0, 0.0], [0.0, 0.0, 0.2, 0.0]]}, {"plan_times": [24, 48, 72], "plans": ["NST", "AG", "AG", "EWT"]}]]');
				// *****

				console.log(d);

				// update general status things
				var general = d[0];
				// var general = d[0]['general'][0];
				pv.$('#stat-status').html(general['status']);

				// change the icons
				pv.$('#dash-state').attr('src', 'assets/' + general['behaviour'].toLowerCase() + '.png');
				pv.$('#dash-next-state').attr('src', 'assets/' + general['plan'].toLowerCase() + '.png');

				// update activity
				pv.activitiesCollection.reset(d[1]['events']);

				// update behaviors
				var flows = d[2]['in']
				var bMat = d[3][0]['bhvr_mtx'];
				var ords = ['n','e','s','w'];
				for(var i = 0; i < 4; i++){
					for(var j = 0; j < 4; j++){
						pv.$('#'+ords[i]+'-'+ords[j]).html(flows[i].length > 0 ? String(Math.floor(bMat[i][j] * flows[i][0][1] * 3600 )) : '-');
					}
				}

				var plans = d[3][1];
				pv.$('#stat-currentState').html(plans['plans'][0]);
				pv.$('#stat-nextState').html(plans['plans'].length >1 ? plans['plans'][1] : '-');

				//handle time nicely
				var stateDuration = Math.round(plans['plan_times'][0]-(new Date).getTime()/1000); //new Date((general['plan_time']-general['bhvr_time']) ).format('i:s');
				pv.$('#stat-duration').html(stateDuration);
				if(plans['plan_times'].length > 0){
					var timeUntilNextState = Math.round((plans['plan_times'][0]-(new Date).getTime()/1000));; //assuming it's in the future
					pv.$('#stat-nextStateTime').html(timeUntilNextState);
				} else {
					pv.$('#stat-nextStateTime').html('--:--');
				}

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
	        	// 	pv.flowPlotData[dir].data = flotSeries;
	        	// }

	        	// add flow data
	        	inFlows = d[2]['in'];
	        	for(var dir = 0; dir < 4; dir++){
	        		pv.flowPlotData[dir].data = inFlows[dir];
	        	}
	        	// add prediction data
	        	predicts = d[2]['in'];
	        	for(var dir = 0; dir < 4; dir++){
	        		pv.flowPlotData[dir+4].data = inFlows[dir];
	        	}

	        	// set the data.
	        	pv.flowPlot.setData(pv.flowPlotData);
	        	pv.flowPlot.setupGrid();
		        pv.flowPlot.draw();

	        	// add perf data
	        	perf = d[2]['out'];
	        	for(var dir = 0; dir < 4; dir++){
	        		pv.perfPlotData[dir].data = perf[dir];
	        	}
	        	// add prediction data
	        	perfPredicts = d[2]['out'];
	        	for(var dir = 0; dir < 4; dir++){
	        		pv.perfPlotData[dir+4].data = perf[dir];
	        	}

	        	// set the data.
	        	pv.perfPlot.setData(pv.perfPlotData);
	        	pv.perfPlot.setupGrid();
		        pv.perfPlot.draw();

				// tell s
				s.localUpdate();
				s.restConnected();
			});

			//data pull on activity for intersection
			$.ajax({
				type: "POST",
				datatype: "JSON",
				contentType: "application/json",
				url: "http://localhost:5000/request_all_events",
				data: JSON.stringify({num_events: 20}),
				async: false
			}).then( function(d){
				console.log('activity data');
				console.log(d);
				// reset the activity collection with the new list of events.
				pv.activitiesCollection.reset(d);

				// tell s
				s.localUpdate();
				s.restConnected();
			});

			//update anyhting non-static.

		}, this.activityUpdateFrequency, this);
	},

	stopInterval: function(){
		clearInterval(this.interval);
	}
});