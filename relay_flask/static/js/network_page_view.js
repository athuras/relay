var NetworkPageView = Backbone.View.extend({
	el: $('#network-page'),

	events: {

	},

	updateFrequency: 5000,

	initialize: function(options){

	},

	render: function(){

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
				npv.$('#stat-decisions').html(String(Math.random()*100));


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