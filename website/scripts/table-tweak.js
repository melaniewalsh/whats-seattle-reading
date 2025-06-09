<script>
$(document).on('init.dt', function(e, settings) {
  const $tbl = $(settings.nTable); // Gets the initialized DataTable table
  const n_fixed = $tbl.find('thead tr:first th.dtfc-fixed-left').length;
  let left = 0;

  $tbl.find('thead tr:last td').slice(0, n_fixed).each(function() {
    $(this).addClass('dtfc-fixed-left').css({
      position: 'sticky',
      left: left + 'px'
    });
    left += $(this).outerWidth();
  });
});

</script>