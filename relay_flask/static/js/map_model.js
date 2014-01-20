var MapModel = Backbone.Model.extend({
	// the model won't attach to anything backend yet
	// stores map styles, layers, etc. so the view has something to listen to.
	defaults: {
		'mapOptions': null,
		'mapStyles': null
	},

	initialize: function(){
		// We set the map styles and map options to be attributes
		this.set('mapStyles', bootstrap.mapStyles);
		this.set('mapOptions', bootstrap.mapOptions);
	}

});