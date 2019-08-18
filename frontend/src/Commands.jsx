import React from "react"
import { Row, Col } from "react-bootstrap"

export const View = props =>
  <div>
    <Row>
        <Col md={3}>Channel</Col>
        <Col md={3}>Command Name</Col>
        <Col md={3}>Command Body</Col>
    </Row>
    {props.commands.length > 0 ?
        props.commands.map((c, _) =>
            <Row>
                <Col md={3}>{c.channel}</Col>
                <Col md={3}>{c.name}</Col>
                <Col md={3}>{c.body}</Col>
            </Row>
        )
        : "No commands found." }
  </div>
