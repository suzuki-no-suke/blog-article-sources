import { useState, useEffect, useRef } from 'react'
import './App.css'

import { Button } from './components/ui/button'
import { Input } from './components/ui/input'
import { Label } from './components/ui/label'

function App() {
  const [flag, setFlag] = useState("");
  const [message, setMessage] = useState("");
  const socketRef = useRef<WebSocket>(null);

  const send_button = () => {
    console.log(`test value is -> ${flag}`)

    socketRef.current?.send(flag);
  }

  // websocket handling
  // connect on boot
  useEffect(() => {
    if (socketRef.current) {
      console.log(socketRef);
      console.log("already connected");
      return
    } else {
      console.log("connection try")
    }

    try {
      const websocket = new WebSocket('ws://localhost:8878/ws')
      socketRef.current = websocket

      // メッセージ受信時のハンドラを設定
      const onOpen = (event) => {
        console.log("ws open")
        console.log(event)
      }
      const onMessage = (event: MessageEvent<string>) => {
        setMessage(event.data)
      }
      const onError = (event) => {
        console.log("ws errored")
        console.log(event)
      }
      const onClose = (event) => {
        console.log("ws closed")
        console.log(event)
      }

      websocket.addEventListener('open', onOpen)
      websocket.addEventListener('message', onMessage)
      websocket.addEventListener('error', onError)
      websocket.addEventListener('close', onClose)

      // // on cleanup
      // return () => {
      //   websocket.close()
      //   websocket.removeEventListener('open', onOpen)
      //   websocket.removeEventListener('message', onMessage)
      //   websocket.removeEventListener('error', onError)
      //   websocket.removeEventListener('close', onClose)
      // }
    } catch {
      console.log("connection error detected");
      return
    }


  }, [socketRef.current])

  return (
    <>
      <h1>WebSocket Test yade</h1>

      <Label htmlFor=''>入力</Label>
      <Input value={flag} onChange={(e) => setFlag(e.target.value)} />
      <Button onClick={send_button} className='text-black-100'>送る</Button>
      <p>Value is : {message}</p>
    </>
  )
}

export default App
