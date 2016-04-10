jQuery(document).ready(function($) {
  var above_breakpoint = function() {
    return $(window).width() > breakpoint;
  }

  var breakpoint = 767,
      was_above_breakpoint = above_breakpoint(),
      split_class = 'responsive-table-split';

  var splitTables = function() {
    $("table.responsive").not('.' + split_class).each(function(i, element) {
      var $table = $(element);

      splitTable($table);

      $table.addClass(split_class);
    });
  }

  var unsplitTables = function() {
    $("table.responsive." + split_class).each(function(i, element) {
      var $table = $(element);

      unsplitTable($table);

      $table.removeClass(split_class);
    });
  }

  $(window).on("load", function() {
    if(!above_breakpoint()) {
      splitTables();
    }
  });

  $(window).on("resize", _.throttle(function() {
    var is_above_breakpoint = above_breakpoint();

    if(is_above_breakpoint && !was_above_breakpoint) {
      unsplitTables();
    }
    else if(!is_above_breakpoint && was_above_breakpoint) {
      splitTables();
    }

    was_above_breakpoint = is_above_breakpoint;
  }, 100));

  /* Allows dynamically-initialized tables to trigger a refresh for responsive tables */
  $(document).on("responsive_tables.new_table", function() {
    if(!above_breakpoint()) {
      splitTables();
    }
  });

	function splitTable(original)
	{
		original.wrap("<div class='table-wrapper' />");
		
		var copy = original.clone();
		copy.find("td:not(:first-child), th:not(:first-child)").css("display", "none");
		copy.removeClass("responsive");
		
		original.closest(".table-wrapper").append(copy);
		copy.wrap("<div class='pinned' />");
		original.wrap("<div class='scrollable' />");

    setCellHeights(original, copy);
	}
	
	function unsplitTable(original) {
    original.closest(".table-wrapper").find(".pinned").remove();
    original.unwrap();
    original.unwrap();
	}

  function setCellHeights(original, copy) {
    var tr = original.find('tr'),
        tr_copy = copy.find('tr'),
        heights = [];

    tr.each(function (index) {
      var self = $(this),
          tx = self.find('th, td');

      tx.each(function () {
        var height = $(this).innerHeight(true);
        heights[index] = heights[index] || 0;
        if (height > heights[index]) heights[index] = height;
      });

    });

    tr_copy.each(function (index) {
      $(this).height(heights[index]);
    });
  }

});
