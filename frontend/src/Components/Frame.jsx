import React from "react"
import { Row, Col, Container, ListGroup, Card } from "react-bootstrap"

export const Main = ({items, inner}) =>
  <Container>
    <Row>
      <Col md={3}>
        <ListGroup>
          {items.map(({title, isSelected, onSelect}) =>
            <ListGroup.Item key={title} active={isSelected} onClick={onSelect} action>
              {title}
            </ListGroup.Item>
          )}
        </ListGroup>
      </Col>
      <Col md={9}>
        <Card>
          <Card.Body>{inner}</Card.Body>
        </Card>
      </Col>
    </Row>
  </Container>