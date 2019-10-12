var page = require('webpage').create();
page.paperSize = {
    format : 'A4',
    orientation : 'portrait',
    margin : {
        top : '1cm',
        left : '1cm',
        bottom : '1cm',
        right : '1cm'
    }
};
page.open('http://localhost:8080/cv.html', function() {
  page.render('cv.pdf');
  phantom.exit();
});
