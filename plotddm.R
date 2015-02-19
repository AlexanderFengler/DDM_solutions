# Plotting outcomes

plotddm = function(){
sum(x$upCrossing)
sum(x$downCrossing)
sum(x$upCrossing) + sum(x$downCrossing)
plot(1:timeMax,x$upCrossing,type="line",col=1)
points(1:timeMax,x$downCrossing,type="line",col=2)
sum((1:timeMax)*x$upCrossing)/sum(x$upCrossing)
sum((1:timeMax)*x$downCrossing)/sum(x$downCrossing)
}