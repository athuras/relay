var PanelView = Backbone.View.extend({

	el: $('#side-panel'),

	events: {
		'click #panel-toggle': 'panelToggled'
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

	activityUpdateFrequency: 5000, // update activity every 5 seconds

	initialize: function(){
		// this.chartOptions = bootstrap.chartOptions;
		// this.chartDataFormats = bootstrap.chartDataFormats;
		this.activitiesCollection = new ActivitiesCollection();

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
		this.$('#panel-container').addClass('expanded');
		this.isExpanded = true;
	},

	collapse: function(){
		// remove all the styling and hide
		this.$('#panel-container').removeClass('expanded');
		this.$el.removeClass('expanded');
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
		this.$('#intersection-title').text( intersectionModel.get('name') );

		// $.ajax({
		// 	type: 'POST',
		// 	datatype: 'JSON',
		// 	contentType: 'application/json',
		// 	url: 'http://localhost:5000/api/charts',
		// 	data: JSON.stringify({}),
		// 	async: false
		// }).then( function(d){
		// 	console.log(d);
		// 	var performance_data = bootstrap.chartDataFormats;
		// 	performance_data.labels = d.time;
		// 	performance_data.datasets[0].data = d.perf;
		// 	performance_data.datasets[1].data = d.vol;

		// 	var p_ctx = document.getElementById("performance-chart").getContext("2d");
		// 	var performanceChart = new Chart(p_ctx).Line(performance_data, this.chartOptions);

		// 	var capacity_data = bootstrap.chartDataFormats;
		// 	capacity_data.labels = d.time;
		// 	capacity_data.datasets[0].data = d.costUD;
		// 	capacity_data.datasets[1].data = d.costLR;

		// 	var c_ctx = document.getElementById("capacity-chart").getContext("2d");
		// 	var capacityChart = new Chart(c_ctx).Line(capacity_data, this.chartOptions);
		// });

		// this.$('#intersection-volume').text( intersectionModel.get('volume') );
		// this.$('#intersection-performance').text( intersectionModel.get('performance') );

		// For anything that requires real-time data, we use a set interval!
		this.startInterval();
	},

	startInterval: function(){
		this.interval = setInterval(function(pv){
			// do your data pull on the intersection

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
			})
			//update anyhting non-static.
		}, this.activityUpdateFrequency, this)
	},

	stopInterval: function(){
		clearInterval(this.interval);
	}
});