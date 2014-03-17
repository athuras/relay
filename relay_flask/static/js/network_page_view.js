var NetworkPageView = Backbone.View.extend({
	el: $('#network-page'),

	events: {

	},

	updateFrequency: 5000,

	activityGridColumns: [{
		cell: 'string',
		name: 'timestamp',
		label: 'Date',
		editable: false,
	},{
		cell: 'string',
		name: 'value',
		label: 'Event',
		editable: false,
	}],

	initialize: function(options){

		// create the backgrid and activities stuff
		this.activitiesCollection = new ActivitiesCollection();
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

	setActive: function(){
		// we start the intervals
		this.startInterval();
	},

	setInactive: function(){
		// stop intervals
		this.stopInterval();
	},

	startInterval: function(){
		// start the data polling and updating to the page
		this.interval = setInterval(function(npv){

			// network stats
			$.ajax({
				type: "GET",
				datatype: "JSON",
				contentType: "application/json",
				url: "http://localhost:5000/request_network",
				async: false
			}).then( function(d){
				console.log(d);

				npv.$('#stat-intersections').html(String(d[0]['num']+d[1]['num']));
				npv.$('#stat-working-count').html(String(d[1]['num']));
				npv.$('#stat-failure-count').html(String(d[0]['num']));
				npv.$('#stat-decisions').html(String(Math.floor(1000+Math.random()*25)));


				// tell s
				s.localUpdate();
				s.restConnected();
			});

			//network events
			$.ajax({
				type: "POST",
				datatype: "JSON",
				contentType: "application/json",
				url: "http://localhost:5000/request_all_events",
				data: JSON.stringify({num_events: 100}),
				async: false
			}).then( function(d){
				console.log('activity data');
				console.log(d);
				// reset the activity collection with the new list of events.
				npv.activitiesCollection.reset(d);

				// tell s
				s.localUpdate();
				s.restConnected();
			});

		}, this.updateFrequency, this);
	},

	stopInterval: function(){
		// stop interval
		clearInterval(this.interval);
	},

});