// ==========================
// last modified: 10-20-2014
// ==========================
function loadData(dataUrl,dataType){
	return $.ajax({
		url: dataUrl,
		dataType: dataType,
		type: "GET",
		success: function(res){
			console.log("data loaded");
			}
		});
};

//
function formatFloat(data){
	if (data !== "NA") return Number(data).toFixed(5);
	if (data == "NA") return "NA";
	};
	
//create DataTable 
function configDataTable(cols,colDefs,data) {
	var config = {	
			"columns": cols,
			"columnDefs":colDefs,
			"data": data,
			"bSelectOnly":true,
			"bProcessing":true,
			"paging":true,
			"bLengthChange":true,
			"sScrollY": "500px",
			"sScrollX": true,
			"autoWidth":true,
			"scrollCollapse": true,
			"jQueryUI": true,
			"dom": 'frtiS',
			};
			
	return config;
};

function configTableTools(){
	config = {
		"sSwfPath":"//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
		"sRowSelect": "os",
		"aButtons": 
			[
				"select_none",
				{
					"sExtends": "copy",
					"oSelectorOpts": { filter: 'applied', order: 'current' }
				},
				{
					"sExtends": "xls",
					"sButtonText": "Save as",
					"oSelectorOpts": { filter: 'applied', order: 'current' },
				},
			],
	};	
	return config;
};

function configColumnFilter(colTypes){
	var colFilter = []
	for (var i in colTypes) {
		if (colTypes[i] == "search") colFilter[i] = {"type":"text",bRegex: true,bSmart: true};
		else if (colTypes[i] == "range")  colFilter[i] = {"type":"number-range"};
		else if (colTypes[i] == null) colFilter[i] = null;
		else colFilter[i] = null;
	};
	return colFilter;
};

function loadJasonToTable(jsonPath,tableId,cols,colTypes,colDefs) {
	var loaded = loadData(jsonPath);
	loaded.then(function(res){
		//create dataTable with data loaded from json

		var dataTableConfig = configDataTable(cols,colDefs,res);	
		var table = $("#"+tableId).dataTable(dataTableConfig);
		var tt = new $.fn.dataTable.TableTools(table,configTableTools());
		$(tt.fnContainer()).insertBefore('div.dataTables_filter');
		
		//create customized column filter/search box above header
		table.columnFilter(            
			{
			sRangeFormat: "{from} {to}",
			sPlaceHolder :  "head:before",
			aoColumns: configColumnFilter(colTypes)
		});
	   
	   //clear search result
	   addRset = '<button type="button" class="clearSearch"> Clear Filters </button>' ;
	   $("#"+tableId+"_filter").append(addRset);
	   $("button.clearSearch").on("click",function(evt){
			$(".text_filter").val("");
			$(".number_range_filter").val("");
			table.fnFilterClear();
		});
	   
	});
};

