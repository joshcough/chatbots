import React from "react"
import { Row, Col } from "react-bootstrap"

export const View = props =>
  <div>
    <Row>
        <Col md={3}>Channel</Col>
        <Col md={3}>Quote Id</Col>
        <Col md={3}>Quote</Col>
    </Row>
    {props.commands.length > 0 ?
        props.commands.map((c, _) =>
            <Row>
                <Col md={3}>{c.channel}</Col>
                <Col md={3}>{c.qid}</Col>
                <Col md={3}>{c.text}</Col>
            </Row>
        )
        : "No commands found." }
  </div>
