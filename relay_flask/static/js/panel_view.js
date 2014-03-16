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