var MapStyleView = Backbone.View.extend({

	tagName: 'p',

	events: {
		'click': 'onSelect'
	},

	initialize: function(){
		// MapStyleModel added automically

		this.model.collection.on('activeChange', this.onActiveChanged, this);
	},

	onSelect: function(){
		this.model.collection.setActive(this.model);
	},

	onActiveChanged: function(newStyle){
		if (this.model === newStyle){
			this.$el.css('color','#fff');
		} else {
			this.$el.css('color','#666');
		}
	},


	render: function(){
		this.$el.text(this.model.get('title'));
		return this; // For chaining
	}

});

// todo:
// when it's clicked, model is activated