var PanelView = Backbone.View.extend({

	el: $('#side-panel'),

	events: {
		'click #panel-toggle': 'panelToggled'
	},

	initialize: function(){

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
		this.$('#intersection-title').text( intersectionModel.get('name') );
		this.$('#intersection-volume').text( intersectionModel.get('volume') );
		this.$('#intersection-performance').text( intersectionModel.get('performance') );
	}
});