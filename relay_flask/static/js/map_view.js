var MapView = Backbone.View.extend({

	// HTML object attacted to this view.
	el: $("#map"),

	// Default values
	defaults: {},

	// Initialize()
	// Creates the google map.
	initialize: function(options) {
		// model is referenced automatically if declared in the options

		// Create the map
		this.map = new google.maps.Map(document.getElementById('map'), this.model.get('mapOptions'));

		// Create the map styles model and handler
		this.mapStyleCollection = new MapStyleCollection(this.model.get('mapStyles'));
		this.mapStyleCollectionView = new MapStyleCollectionView({model: this.mapStyleCollection, map: this.map});

		// Add listeners
		// When a new style is selected, we apply it to the map
		this.mapStyleCollection.on('activeChange', this.onMapStyleChange, this);

		// Set the default map style
		this.mapStyleCollection.setActive(this.mapStyleCollection.findWhere({title:'light'})); // set one to be active by default
	},

	// getMap()
	//Public access to the gmaps javascript object.
	getMap: function(){
		return this.map;
	},

	// onMapStyleChange()
	// if the map style changes in the model, the map updates to reflect the new style
	onMapStyleChange: function(newStyle){
		this.map.setOptions({styles: newStyle.get('styleArray')});
	}

})