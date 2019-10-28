import React from "react"
import { Table } from "react-bootstrap"

export const View = props =>
  <div style={{ backgroundColor: 'black', color: 'white', width: '325px' }}>
    { props.messages.map((cm, _) =>
      <div> {cm.user}: {cm.body.map((b, _) => b)} </div>
    )}
  </div>
