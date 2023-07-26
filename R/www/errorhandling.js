$(document).ajaxError(function myErrorHandler(event, xhr, ajaxOptions, thrownError) {
  alert("Caught Ajax error. You have likely been logged out automatically - refresh the page to continue.\n" + thrownError);
});

$.fn.dataTable.ext.errMode = function ( settings, helpPage, message ) { 
  alert("Caught DataTable error. You have likely been logged out automatically - refresh the page to continue.\n" + message);
};