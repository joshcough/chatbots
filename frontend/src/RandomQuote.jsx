import React from "react"
import { Dropdown, Table } from "react-bootstrap"

export const View = props =>
  <div>{props.quote ? <div>Quote #{props.quote.qid}: {props.quote.body}</div> : <div></div> }
  </div>
