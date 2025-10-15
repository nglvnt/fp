import carotte
import carotte/channel
import carotte/queue
import gleam/erlang/process
import gleam/io

fn callback(msg: queue.Payload, _deliver: queue.Deliver) -> Nil {
  io.println(" [x] Received " <> msg.payload)
}

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

  // subscribe to messages
  // messages are auto-acknowledged by default
  let assert Ok(_) =
    queue.subscribe(channel: channel, queue: "hello", callback: callback)

  io.println(" [*] Waiting for messages.")
  process.sleep_forever()

  // clean up
  let assert Ok(_) = carotte.close(client)
}
