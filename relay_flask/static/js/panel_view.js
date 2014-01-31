var PanelView = Backbone.View.extend({

	el: $('#side-panel'),

	events: {
		'click #panel-toggle': 'panelToggled'
	},

	initialize: function(){
		this.chartOptions = bootstrap.chartOptions;
		this.chartDataFormats = bootstrap.chartDataFormats;
	},

	render: function(){

	},

	panelToggled: function(){
		switch(this.$el.hasClass('expanded')){
			case(true):
				this.$('#panel-container').css('display', 'none');
				this.$el.animate({
					'min-width': '16px',
					width: '16px'
				}, 400);
				this.$('#panel-toggle').css({
					'transform' : 'rotate(180deg)',
					'-ms-transform' : 'rotate(180deg)', /* IE 9 */
					'-webkit-transform' : 'rotate(180deg)' /* Safari and Chrome */
				})
				this.$el.removeClass('expanded');
				break;

			case(false):
				this.$el.animate({
					'min-width': '238px',
					width: '33%'
				}, 400);
				this.$('#panel-toggle').css({
					'transform' : 'rotate(0deg)',
					'-ms-transform' : 'rotate(0deg)', /* IE 9 */
					'-webkit-transform' : 'rotate(0deg)' /* Safari and Chrome */
				})
				setTimeout(function(){
					this.$('#panel-container').css('display', 'block');
				}, 400);
				this.$el.addClass('expanded');
				break;

			default:
				break;
		}
	},

	// populate the intersection name and info
	showIntersectionDetails: function(intersectionModel){
		if(!this.$el.hasClass('expanded')){
			this.panelToggled;
		}

		this.$('#intersection-title').text( intersectionModel.get('name') );

		$.ajax({
			type: 'POST',
			datatype: 'JSON',
			contentType: 'application/json',
			url: 'http://localhost:5000/api/charts',
			data: JSON.stringify({}),
			async: false
		}).then( function(d){
			console.log(d);
			var performance_data = bootstrap.chartDataFormats;
			performance_data.labels = d.time;
			performance_data.datasets[0].data = d.perf;
			performance_data.datasets[1].data = d.vol;

			var p_ctx = document.getElementById("performance-chart").getContext("2d");
			var performanceChart = new Chart(p_ctx).Line(performance_data, this.chartOptions);

			var capacity_data = bootstrap.chartDataFormats;
			capacity_data.labels = d.time;
			capacity_data.datasets[0].data = d.costUD;
			capacity_data.datasets[1].data = d.costLR;

			var c_ctx = document.getElementById("capacity-chart").getContext("2d");
			var capacityChart = new Chart(c_ctx).Line(capacity_data, this.chartOptions);
		});

		// this.$('#intersection-volume').text( intersectionModel.get('volume') );
		// this.$('#intersection-performance').text( intersectionModel.get('performance') );
	}
});