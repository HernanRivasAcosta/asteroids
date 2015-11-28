// The ship model has 2 versions, with and without thrust
var shipModelA = [[new Point(0, -15), new Point(6, 5), new Point(-6, 5), new Point(0, -15)]];
// The version with thrust extends the basic model adding a few vectors
var shipModelB = [shipModelA[0], [new Point(-4, 5), new Point(0, 12), new Point(4, 5)]];