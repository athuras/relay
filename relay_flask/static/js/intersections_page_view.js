var IntersectionsPageView = Backbone.View.extend({
	gridColumns:[{
		cell: 'string',
		name: 'name',
		label: 'Name',
		editable: false,
	},{
		cell: 'string',
		name: 'type',
		label: 'type_short',
		editable: false,
	}],

	el: $('#intersections-page'),

	events: {

	},

	initialize: function(options){
		this.intersectionsCollection = options.ic;

		// Create the grid
		this.intersectionsTable = new Backgrid.Grid({
			columns: this.gridColumns,
			collection: this.intersectionsCollection
		});

		// Create the filter
		this.intersectionsFilter = new Backgrid.Extension.ClientSideFilter({
			collection: this.intersectionsCollection,
			fields:['name'],
		});

		this.render();
		this.$('#intersections-table-container').append(this.intersectionsTable.el);
		this.$('#intersections-filter-container').append(this.intersectionsFilter.el);
	},

	render: function(){
		this.intersectionsTable.render();
		this.intersectionsFilter.render();
	},


});