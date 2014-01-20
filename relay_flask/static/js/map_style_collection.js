var MapStyleCollection = Backbone.Collection.extend({

	model: MapStyleModel,

	initialize: function(){
		// this._meta = {
		// 	'activeStyle': null
		// };
	},

	setMeta: function(property, value){
		this._meta[property] = value;
		this.trigger('metaChange:' + property, value);
	},

	getMeta: function(property){
		return this._meta[property];
	},

	// setActive()
	// If a different stlye is picked to be active, we send out a notification with the new style
	// setActive: function(newStyle){
	// 	this.trigger('activeChange', newStyle);
	// }

});