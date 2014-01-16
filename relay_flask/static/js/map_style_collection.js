var MapStyleCollection = Backbone.Collection.extend({

	model: MapStyleModel,

	initialize: function(){
	},

	// setActive()
	// If a different stlye is picked to be active, we send out a notification with the new style
	setActive: function(newStyle){
		this.trigger('activeChange', newStyle);
	}

});