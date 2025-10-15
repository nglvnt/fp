# Get started with RabbitMQ in Gleam

## Introduction

## Installing and running RabbitMQ

## Hello world

As an introduction, we create the *Hello World* equivalent for RabbitMQ. It consists of a producer, a consumer, and a queue between them: the producer sends messages to the queue, and the consumer receives messages from that queue.

$\text{producer} \longrightarrow \text{queue} \longrightarrow \text{consumer}$

Let's start with the producer program, that sends a message to the queue. First, we need to have a connection with the RabbitMQ server: we establish a client connection and then open a channel to the server.

In Gleam, we can use the `carotte` package. The `Client` creation uses the builder pattern: we start with a client `Builder`, called `default_client()`, set its properties to the desired ones, and then the `start` function is used to get the `Client`. Once we have the `Client`, we open a `Channel` to the server.

```gleam
let assert Ok(client) =
  carotte.default_client()
  |> carotte.with_host("localhost")
  |> carotte.start()

let assert Ok(channel) = channel.open_channel(client)
```

The producer sends the message to a queue, and we have to make it sure that the queue exists, as messages sent to a non-existing queue are dropped. Thus we declare a queue `hello` on the broker.

```gleam
let assert Ok(_) =
  queue.new("hello")
  |> queue.declare(channel)
```

Our producer-queue-consumer model is a simplified version of how RabbitMQ really works: in RabbitMQ, messages are usually sent to an *exchange*, however we will deal with exchanges later. For now, the default (or nameless) exchange will be used, defined by the empty string "". Its special property is that we can specify to which queue we would like to send the message to.

The last step is to publish the message. Our message is just "Hello World!", and we send it to the `hello` queue, through the default exchange.

```gleam
let assert Ok(_) =
  publisher.publish(
    channel: channel,
    exchange: "",
    // for the default exchange, routing_key specifies the queue
    routing_key: "hello",
    payload: "Hello World!",
    options: [],
  )
```

To clean up, we close the connection.

```gleam
let assert Ok(_) = carotte.close(client)
```

The complete producer code can be found in the [`hello_world_producer.gleam`](fp-gleam/src/rabbitmq/hello_world_producer.gleam) file.

We are continuing with the consumer program, which receives and processes the message by printing it.

The consumer needs to have a connection to the broker, for which we can use the same code as we have for the producer.

We also need to make sure that the queue exists. Queue declaration is idempotent, so no matter how many times we run it, it will only produce one queue. Of course, if we know that the queue exists, we can skip this, but in general, it is a good practice to declare the queue.

Message receiving works by subscribing to the queue a `callback` function, which is called every time a message is received. In this case, we choose to print the contents of the message to stdout.

```gleam
fn callback(msg: queue.Payload, _deliver: queue.Deliver) -> Nil {
  io.println("Received: " <> msg.payload)
}
```

```gleam
let assert Ok(_) =
  queue.subscribe(channel: ch, queue: "hello", callback: callback)
```

We end by suspending the main process forever, so that the `callback` function can run when a message is received.

```gleam
process.sleep_forever()

let assert Ok(_) = carotte.close(client)
```

The complete code can be found in the [`hello_world_consumer.gleam`](fp-gleam/src/rabbitmq/hello_world_consumer.gleam) file.

Let's run our programs in the terminal. We start the consumer in a terminal, which will be continuously waiting for the messages.

```shell
$ gleam run -m rabbitmq/hello_world_consumer
...
 [*] Waiting for messages.
```

In a second terminal, we run the producer, which will stop after sending the message.

```shell
$ gleam run -m rabbitmq/hello_world_producer
...
 [x] Sent 'Hello World!'
```

In the first terminal, we see that the message was received and printed by the `callback` function.

```shell
$ gleam run -m rabbitmq/hello_world_consumer
...
 [*] Waiting for messages.
 [x] Received Hello World!
```

## Packages

Carotte <https://hexdocs.pm/carotte/> <https://github.com/renatillas/carotte/>

## Resources

<https://www.rabbitmq.com/tutorials>

<https://www.rabbitmq.com/tutorials/tutorial-one-python>
