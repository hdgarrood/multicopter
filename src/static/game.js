function upto(i, j) {
    var arr = [],
        val = i

    while (val <= j) {
        arr.push(val)
        val += 1
    }

    return arr
}

function eachIndex(arr, callback) {
    _.each(upto(0, arr.length - 1), callback)
}

function drawWorld(world, canvas) {
	var context = canvas.getContext('2d'),
		width = canvas.width,
		slices = world.slices,
		offset = world.offset,
		sliceWidth = 30

	eachIndex(slices, function(i) {
        drawSlice(slices[i], context, sliceWidth,
            { x: sliceWidth * i - offset, y: 0 })
    })
}

function drawSlice(slice, context, width, pos) {
    var isInsideObstacle = true,
        xStart = _.max([0, pos.x]),
        xEnd = xStart + width - 1,
        currentY = pos.y

    _.each(slice, function(boundary) {
        context.fillStyle = isInsideObstacle ? "green" : "white"
        context.fillRect(xStart, currentY, xEnd, boundary)

        isInsideObstacle = !isInsideObstacle
        currentY = boundary
    })

    context.fillStyle = isInsideObstacle ? "green" : "white"
    context.fillRect(xStart, currentY, xEnd, context.canvas.height)
}

function getWebSocketUrl() {
    var hostname = window.location.hostname,
        port = "9160",
        path = $('#canvas-container').attr('data-websocket-path')

    return ["ws://", hostname, ":", port, path].join('')
}

function shiftPush(arr, item) {
    arr.shift()
    arr.push(item)
}

function keyEq(key) {
    return function(val) {
        return function(obj) {
            obj[key] == val
        }
    }
}

function getOne(pred) {
    return function(arr) {
        var arr2 = _.filter(arr, pred)

        if (arr2.length == 1)
            return arr2[0]
        else
            throw new Error("getOne: array was not length 1")
    }
}

function start() {
    var maxSlicesInWorld = 28,
        world = {
            slices: replicate(maxSlicesInWorld, [0]),
            offset: 0,
            helis: []
        },
        wsUrl = getWebSocketUrl()

    var ws = new WebSocket(wsUrl)
    ws.addEventListener("open", function(e) { console.log("open!") })
    ws.addEventListener("error", function(e) { throw e })
    ws.addEventListener("close", function(e) { console.log("closed." })

    ws.addEventListener("message", function(e) {
        var changes = JSON.parse(e.data)

        _.each(changes, function(c) {
            switch c.type {
            case "GameStarted":
                break // todo
            case "GameFinished":
                break // todo
            case "WorldChange":
                var inner = c.data
                switch inner.type {
                case "SliceAdded":
                    shiftPush(world.slices, inner.data)
                    break
                case "SlicesMoved":
                    world.offset = inner.data
                }
                break
            case "HeliChange":
                var inner = c.data
                switch inner.type {
                case "HeliAdded":
                    world.helis.push(inner.data)
                    break
                case "HeliMoved":
                    var h = getOne(world.helis, keyEq("id")(inner.data["id"]))
                    h.position += inner.data["dist"]
                case "HeliCrashed":
                    var h = getOne(world.helis, keyEq("id")(inner.data["id"]))
                    h.isAlive = false
                }
                break
            }
        })
    })
}
