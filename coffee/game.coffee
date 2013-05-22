drawWorld = (world, canvas) ->
    context    = canvas.getContext('2d')
    width      = canvas.width
    slices     = world.slices
    offset     = world.offset
    sliceWidth = width / slices.length

    [0..slices.length - 1].forEach (i) ->
        drawSlice(slices[i],
                  context,
                  sliceWidth,
                  { x: (sliceWidth * i) - offset, y: 0 })

drawSlice = (slice, context, width, position) ->
    isInsideObstacle = true
    xStart = position.x
    xEnd = xStart + width - 1
    currentY = position.y

    slice.forEach (boundary) ->
        context.fillStyle = if isInsideObstacle then "green" else "white"
        context.fillRect(xStart, currentY, xEnd, boundary)

        isInsideObstacle = !isInsideObstacle
        currentY = boundary

    context.fillStyle = if isInsideObstacle then "green" else "white"
    context.fillRect(xStart, currentY, xEnd, context.canvas.height)

# Create an Array containing 'n' copies of 'value'.
replicate = (n, value) ->
    Array.apply(null, {length: n}).map(-> value)

Bacon.fromWebSocket = (webSocket) ->
    Bacon.fromEventTarget(webSocket, "message").map(".data")

# Wraps data received from the server to make it easier to process.
class WorldChanges
    constructor: (@changes) ->

    _getChange: (name) =>
        _.find(@changes, (change) -> change.type == name)

    isSliceAdded: =>
        @_getChange('sliceAdded') != undefined

    newSlice: =>
        @_getChange('sliceAdded').data

    newWorldOffset: =>
        @_getChange('slicesMoved').data


start = ->
    interval = 120
    worldWidth = 30
    initialWorld =
        slices: replicate(worldWidth, [0])
        offset: 0

    webSocket = new WebSocket("ws://#{window.location.hostname}:9160/")
    webSocket.onopen  =       -> console.log "websocket opened"
    webSocket.onerror = (msg) -> console.log "Cripes! A websocket error: #{msg.toString()}"
    webSocket.onclose =       -> console.log "websocket closed"

    world = Bacon.fromWebSocket(webSocket)
            .map((data) -> new WorldChanges(JSON.parse(data)))
            .scan(initialWorld, (world, change) ->
                if change.isSliceAdded()
                    world.slices.shift()
                    world.slices.push(change.newSlice())

                world.offset = change.newWorldOffset()

                world
            )
    canvas = document.getElementById('canvas')

    world.onValue((world) ->
        drawWorld(world, canvas)
    )

window.start = start
