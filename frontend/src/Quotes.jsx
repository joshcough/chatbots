import React from "react"
import { Row, Col } from "react-bootstrap"

export const View = props =>
  <div>
    <Row>
        <Col md={3}>Channel</Col>
        <Col md={3}>Quote Id</Col>
        <Col md={3}>Quote</Col>
    </Row>
    {props.quotes.length > 0 ?
        props.quotes.map((q, _) =>
            <Row>
                <Col md={3}>{q.channel}</Col>
                <Col md={3}>{q.qid}</Col>
                <Col md={3}>{q.body}</Col>
            </Row>
        )
        : "No quotes found." }
  </div>
