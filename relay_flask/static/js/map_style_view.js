var MapStyleView = Backbone.View.extend({

	tagName: 'p',

	events: {
		'click': 'onSelect'
	},

	initialize: function(){
		// MapStyleModel added automically
		// this.model.collection.on('metaChange:activeStyle', this.onActiveChanged, this);
		this.model.collection.on('change:isActive', this.onActiveChanged, this);
		this.onActiveChanged();
	},

	// onSelect()
	// If the ui piece is selected, we notify the collection
	onSelect: function(){
		// this.model.collection.setMeta('activeStyle', this.model);
		this.model.set('isActive', true);
	},

	// onActiveChanged()
	// Responds to the active style being changed in the collection, and decorates the UI piece appropriately.
	onActiveChanged: function(){
		if ( this.model.get('isActive') ){
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