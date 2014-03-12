var AppView = Backbone.View.extend({

	// HTML object associated with AppView
	el: $('#app'),

	// Other references:
	header: $('#header'),

	activeTab: $('#map-header-btn'),

	mapHeaderBtn: $('#map-header-btn'),
	intersectionsHeaderBtn: $('#intersections-header-btn'),
	roadsHeaderBtn: $('#roads-header-btn'),
	networkHeaderbtn: $('#network-header-btn'),

	events: {
		'click .page-btn' : 'pageSelected',
	},

	// initialize()
	// starts the app
	initialize: function() {

		// Get pages
		this.mapPageView = new MapPageView();
		this.intersectionsPageView = new IntersectionsPageView();
		this.roadsPageView = new RoadsPageView();
		this.networkPageView = new NetworkPageView();

		this.render();
	},

	render: function(){

	},

	pageSelected: function(e){
		this.setPage($('#' + e.target.id));
	},

	setPage: function(pageTab){
		this.activeTab.removeClass('active'); // unselect the active tab
		$( '#' + this.activeTab.data('page-id')).removeClass('page-active'); // hide the active tab

		this.activeTab = pageTab;

		this.activeTab.addClass('active'); // select the new tab
		$( '#' + this.activeTab.data('page-id')).addClass('page-active'); // show the new tab

	}
});