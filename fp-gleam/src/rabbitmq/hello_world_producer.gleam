import carotte
import carotte/channel
import carotte/publisher
import carotte/queue
import gleam/io

pub fn main() {
  // connect to local RabbitMQ
  let assert Ok(client) =
    carotte.default_client()
    |> carotte.with_host("localhost")
    |> carotte.start()

  // open a channel
  let assert Ok(channel) = channel.open_channel(client)

  // declare a queue
  let assert Ok(_) =
    queue.new("hello")
    |> queue.declare(channel)

  // publish message
  let assert Ok(_) =
    publisher.publish(
      channel: channel,
      exchange: "",
      // for the default exchange, routing_key specifies the queue
      routing_key: "hello",
      payload: "Hello World!",
      options: [],
    )

  // print success message
  io.println(" [x] Sent 'Hello World!'")

  // clean up
  let assert Ok(_) = carotte.close(client)
}
