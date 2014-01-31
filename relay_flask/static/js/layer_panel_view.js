var LayerPanelView = Backbone.View.extend({

	el: $(#'layer-container'),

	events:{
		'onmouseover': 'onMouseOver',
		'onmouseout': 'onMouseOut'
	},

	initialize: function(){
		this.render();
	},

	onMouseOver: function(){
		this.el.height(100);
		this.el.width(100);
	}

	onMouseOut: function(){
		this.el.height(50);
		this.el.width(50);
	}

	render: function(){
		return this;
	}
});