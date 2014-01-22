var PanelView = Backbone.View.extend({

	el: $('#panel-container'),

	initialize: function(){

	},

	render: function(){

	},

	// populate the intersection name and info
	showIntersectionDetails: function(intersectionModel){
		this.$('#intersection-name').text( intersectionModel.get('name') );
		this.$('#intersection-volume').text( intersectionModel.get('volume') );
		this.$('#intersection-performance').text( intersectionModel.get('performance') );
	}
});